{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
import RIO
import RIO.Orphans
import Pantry hiding (Package (..))
import RIO.Directory
import RIO.FilePath
import qualified RIO.Map as Map
import qualified RIO.HashMap as HM
import RIO.Process
import Data.Yaml (Value (..), decodeEither')
import Options.Applicative.Simple hiding (header, value)
import qualified Paths_mega_sdist as Paths (version)
import qualified RIO.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified RIO.Text as T
import qualified RIO.List as L
import Path (parseAbsDir)
import Path.IO (resolveFile')

#if MIN_VERSION_aeson(2, 0, 0)
import qualified Data.Aeson.KeyMap
#endif

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appResourceMap :: !ResourceMap
  , appPantryConfig :: !PantryConfig
  }
instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
instance HasResourceMap App where
  resourceMapL = lens appResourceMap (\x y -> x { appResourceMap = y })
instance HasPantryConfig App where
  pantryConfigL = lens appPantryConfig (\x y -> x { appPantryConfig = y })

getPaths :: MonadIO m => Value -> m [FilePath]
getPaths value = maybe (error $ "getPaths failed: " ++ show value) return $ do
    Object top <- return value
    Object locals <- mylookup "locals" top
    forM (toList locals) $ \l -> do
        Object o <- return l
        String path <- mylookup "path" o
        return $ T.unpack path
  where
#if MIN_VERSION_aeson(2, 0, 0)
  mylookup = Data.Aeson.KeyMap.lookup
#else
  mylookup = HM.lookup
#endif

data Args = Args
    { toTag :: !Bool
    , getDiffs :: !Bool
    , rawDirs :: ![FilePath]
    , verbose :: !Bool
    }

main :: IO ()
main = do
    (args@Args {..}, ()) <- simpleOptions
        $(simpleVersion Paths.version)
        "Check Haskell cabal package versions in a mega-repo"
        "Determines if the code present in this repo is the most current with Hackage"
        (Args
            <$> switch
                    ( long "gittag"
                   <> help "Call 'git tag' if all versions are ready for release"
                    )
            <*> switch
                    ( long "get-diffs"
                   <> help "Display the diffs between old and new version"
                    )
            <*> many
                    ( strArgument (metavar "DIR")
                    )
            <*> switch
                    ( long "verbose"
                   <> help "Enable verbose output"
                    )
        )
        empty

    lo <- logOptionsHandle stdout verbose
    pc <- mkDefaultProcessContext
    stack <- liftIO $ getAppUserDataDirectory "stack"
    root <- liftIO $ parseAbsDir $ stack </> "pantry"
    withLogFunc lo $ \lf -> withResourceMap $ \rm ->
      runRIO lf $
      withPantryConfig
        root
        defaultPackageIndexConfig
        HpackBundled
        8
        defaultCasaRepoPrefix
        defaultCasaMaxPerRequest
        defaultSnapshotLocation
#if MIN_VERSION_pantry(0, 10, 0)
        defaultGlobalHintsLocation
#endif
        $ \pantryConfig -> do
            let app = App
                  { appLogFunc = lf
                  , appProcessContext = pc
                  , appResourceMap = rm
                  , appPantryConfig = pantryConfig
                  }
            runRIO app $ main2 args

main2 :: Args -> RIO App ()
main2 Args {..} = do
    (queryBS, _) <- proc "stack" ["query"] readProcess_
    queryValue <-
        case decodeEither' (BL.toStrict queryBS) of
            Left e -> error $ "Could not parse 'stack query': " ++ show e
            Right x -> return x
    allDirs <- getPaths queryValue
    dirs <-
      if null rawDirs
        then return allDirs
        else do
          myPaths <- mapM (fmap addTrailingPathSeparator . canonicalizePath) rawDirs
          return $ filter (\y -> any (\x -> (x `L.isPrefixOf` y)) myPaths) (map addTrailingPathSeparator allDirs)

    whenM (doesDirectoryExist "tarballs") $ removeDirectoryRecursive "tarballs"
    createDirectoryIfMissing True "tarballs"

    tarballs <- forM dirs $ \dir -> do
        output <- proc "stack" ["--no-terminal", "sdist", dir] readProcessStderr_
        case decodeUtf8' $ BL.toStrict output of
          Left e -> error $ "Invalid non-UTF8 output: " ++ show e
          Right text -> case fmap T.unpack $ filter (".tar.gz" `T.isSuffixOf`) $ T.words text of
            fp:_ -> do
                let dest = "tarballs" </> takeFileName fp
                renameFile fp dest
                return dest
            [] -> error $ "Unexpected 'stack sdist' output in dir: " ++ dir ++ "\n\n" ++ T.unpack text

    m <- Map.unionsWith mappend <$> mapM (go getDiffs) tarballs

    case Map.lookup NoChanges m of
        Nothing -> return ()
        Just s -> do
            logInfo "The following packages from Hackage have not changed:"
            mapM_ sayPackage $ Map.keys s
            mapM_ (removeFile . packageFile) $ Map.keys s

    toColor <- hIsTerminalDevice stdout

    case Map.lookup DoesNotExist m of
        Nothing -> return ()
        Just s -> do
            logInfo "\nThe following new packages exist locally:"
            forM_ (Map.toList s) $ \(name, mdiff) -> do
                sayPackage name
                sayDiff toColor mdiff

    case Map.lookup NeedsVersionBump m of
        Nothing -> do
            logInfo "\nNo version bumps required, good to go!"
            when toTag $ forM_ (maybe [] Map.keys $ Map.lookup DoesNotExist m)
              $ \(Package _fp name version) ->
                let ident = packageIdentifierString $ PackageIdentifier name version
                    msg = "Release: " <> ident
                 in proc "git" ["tag", "-s", ident, "-m", msg] $ \pc -> do
                      logInfo $ display pc
                      runProcess_ pc
        Just s -> do
            logInfo "\nThe following packages require a version bump:"
            forM_ (Map.toList s) $ \(name, mdiff) -> do
                sayPackage name
                sayDiff toColor mdiff

sayDiff :: Bool -- ^ use color?
        -> Maybe Diff -> RIO App ()
sayDiff toColor = mapM_ $ BL.hPut stdout . (if toColor then colorize else id)

data Status = DoesNotExist | NoChanges | NeedsVersionBump
    deriving (Show, Eq, Ord)

type Diff = LByteString

go :: Bool -- ^ get diffs
   -> FilePath
   -> RIO App (Map Status (Map Package (Maybe Diff)))
go getDiffs fp = do
    package <- parsePackage fp
    (status, mdiff) <- compareTGZ getDiffs (packageName package) (packageVersion package) fp
    return $ Map.singleton status $ Map.singleton package mdiff

data Package = Package
    { packageFile :: !FilePath
    , packageName :: !PackageName
    , packageVersion :: !Version
    }
    deriving (Show, Eq, Ord)

parsePackage :: MonadThrow m => FilePath -> m Package
parsePackage fp =
    case T.stripSuffix ".tar.gz" $ T.pack $ takeFileName fp of
        Nothing -> error $ "Does not end with .tar.gz: " ++ fp
        Just s ->
            case parsePackageIdentifier $ T.unpack s of
                Nothing -> error $ "Invalid package identifier: " ++ T.unpack s
                Just (PackageIdentifier name version) -> pure $ Package fp name version

sayPackage :: Package -> RIO App ()
sayPackage (Package _ name version) =
  logInfo $ fromString $ packageIdentifierString $ PackageIdentifier name version

compareTGZ :: Bool -- ^ get diffs?
           -> PackageName
           -> Version
           -- ^ version
           -> FilePath
           -- ^ new tarball
           -> RIO App (Status, Maybe Diff)
compareTGZ getDiffs pn v b = withSystemTempDirectory "diff" $ \diff -> do
    oldDest <- parseAbsDir $ diff </> "old"
    newDest <- parseAbsDir $ diff </> "new"
    bAbs <- resolveFile' b

    let pirOrig = PackageIdentifierRevision pn v (CFIRevision (Revision 0))
    let mkPli pir = RPLIHackage pir Nothing
    exists <- (True <$ unpackPackageLocationRaw oldDest (mkPli pirOrig)) `catch` \e ->
      case e of
        UnknownHackagePackage{} -> do
          mpir <- getLatestHackageVersion YesRequireHackageIndex pn UsePreferredVersions
          for_ mpir $ unpackPackageLocationRaw oldDest . mkPli
          pure False
        _ -> throwIO e

    unpackPackageLocationRaw newDest $ RPLIArchive
      RawArchive
        { raLocation = ALFilePath $ ResolvedPath (RelFilePath $ fromString b) bAbs
        , raHash = Nothing
        , raSize = Nothing
        , raSubdir = ""
        }
      RawPackageMetadata
        { rpmName = Nothing
        , rpmVersion = Nothing
        , rpmTreeKey = Nothing
        }

    (ec, out) <- withWorkingDir diff $ proc "diff"
      [ "-ruNw"
      , "old"
      , "new"
      ]
      readProcessStdout
    let status
          | ec == ExitSuccess = NoChanges
          | exists = NeedsVersionBump
          | otherwise = DoesNotExist
        mdiff
          | getDiffs && ec /= ExitSuccess = Just out
          | otherwise = Nothing
    return (status, mdiff)

colorize :: LByteString -> LByteString
colorize =
    BL.intercalate "\n" . map colorLine . BL.split 10
  where
    colorLine :: LByteString -> LByteString
    colorLine l =
      case fst <$> BL8.uncons l of
        Just '-' -> add "31" l
        Just '+' -> add "32" l
        Just '@' -> add "34" l
        _ -> l

    add :: LByteString -> LByteString -> LByteString
    add color l = mconcat
      [ "\x1b["
      , color
      , "m"
      , l
      , "\x1b[0m"
      ]
