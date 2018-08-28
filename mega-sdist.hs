{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
import RIO
import RIO.Orphans
import Pantry hiding (Package (..))
import Conduit
import RIO.Directory
import RIO.FilePath
import qualified RIO.Map as Map
import qualified RIO.HashMap as HM
import Network.HTTP.Simple
import Data.Conduit.Tar
import Data.Conduit.Zlib (ungzip)
import RIO.Process
import Data.Conduit.Binary (sinkFileCautious)
import Data.Yaml (Value (..), decodeEither')
import Options.Applicative.Simple hiding (header, value)
import qualified Paths_mega_sdist as Paths (version)
import qualified RIO.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Semigroup (Max (..), Option (..))
import qualified Data.Version
import Text.ParserCombinators.ReadP (readP_to_S)
import qualified RIO.Text as T
import qualified RIO.List as L
import RIO.Text.Partial (splitOn)
import Path (parseAbsDir)

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
    Object locals <- HM.lookup "locals" top
    forM (toList locals) $ \l -> do
        Object o <- return l
        String path <- HM.lookup "path" o
        return $ T.unpack path

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
        defaultHackageSecurityConfig
        HpackBundled
        8
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
    let v = packageVersion package
        pli = PLIHackage
          (PackageIdentifierRevision (packageName package) v (CFIRevision (Revision 0)))
          Nothing
    (isDiff, mdiff) <- compareTGZ getDiffs (packageName package) pli v fp v
    logDebug $ "Finished calling compareTGZ, isDiff == " <> displayShow isDiff
    let status = if isDiff then NeedsVersionBump else NoChanges
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
           -> PackageLocationImmutable
           -- ^ old contents
           -> Version
           -- ^ old version
           -> FilePath
           -- ^ new tarball
           -> Version
           -- ^ new version
           -> RIO App (Bool, Maybe Diff)
compareTGZ getDiffs pn a av b bv = withSystemTempDirectory "diff" $ \diff -> do
    oldDest <- parseAbsDir $ diff </> "old"
    unpackPackageLocation oldDest a

    let fill dir x = forM_ (Map.toList x) $ \(fp, bs) -> do
          let fp' = dir </> fp
          createDirectoryIfMissing True $ takeDirectory fp'
          BL.writeFile fp' bs
    getContents b >>= fill (diff </> "new")

    let toNV = packageIdentifierString . PackageIdentifier pn
    (ec, out) <- withWorkingDir diff $ proc "diff"
      [ "-ruN"
      , "old" </> toNV av
      , "new" </> toNV bv
      ]
      readProcessStdout
    let isDiff = ec /= ExitSuccess
        mdiff
          | getDiffs && isDiff = Just out
          | otherwise = Nothing
    return (isDiff, mdiff)
  where
    getContents :: FilePath -> RIO App (Map FilePath LByteString)
    getContents fp = handleAny (onErr fp) $ runConduitRes
         $ sourceFile fp
        .| ungzip
        .| untarChunks
        .| withEntries addEntry
        .| foldC

    onErr fp e = do
      logInfo $
        "Error opening tarball: " <>
        fromString fp <>
        ", " <>
        display e
      return mempty

    addEntry header
        | headerFileType header == FTNormal = do
            lbs <- sinkLazy
            yield $ Map.singleton (headerFilePath header) lbs
        | otherwise = return ()

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
