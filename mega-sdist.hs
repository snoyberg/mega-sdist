{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
import RIO
import RIO.Orphans
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

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appResourceMap :: !ResourceMap
  }
instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
instance HasResourceMap App where
  resourceMapL = lens appResourceMap (\x y -> x { appResourceMap = y })

getUrlHackage :: MonadIO m => Package -> m Request
getUrlHackage (Package _fp (PackageName a) (Version b)) =
    liftIO $ parseRequest $ T.unpack $ mconcat
        [ "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"
        , a
        , "-"
        , b
        , ".tar.gz"
        ]

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
    withLogFunc lo $ \lf -> withResourceMap $ \rm -> do
      let app = App
            { appLogFunc = lf
            , appProcessContext = pc
            , appResourceMap = rm
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
        output <- proc "stack" ["sdist", dir] readProcessStderr_
        case decodeUtf8' $ BL.toStrict output of
          Left e -> error $ "Invalid non-UTF8 output: " ++ show e
          Right text -> case fmap T.unpack $ mapMaybe (T.stripPrefix "Wrote sdist tarball to ") $ T.lines text of
            fp:_ -> do
                let dest = "tarballs" </> takeFileName fp
                renameFile fp dest
                return dest
            [] -> error $ "Unexpected 'stack sdist' output in dir: " ++ dir

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
              $ \(Package _fp (PackageName name) (Version version)) ->
                let ident = T.unpack $ mconcat [name, "-", version]
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
    localFileHackage <- getHackageFile package
    localFileExists <- doesFileExist localFileHackage
    let handleFile :: RIO App (Status, Maybe Diff)
        handleFile = do
            let v = packageVersion package
            (isDiff, mdiff) <- compareTGZ getDiffs (packageName package) localFileHackage v fp v
            return $ if isDiff then (NeedsVersionBump, mdiff) else (NoChanges, Nothing)
    (status, mdiff) <-
        if localFileExists
            then handleFile
            else do
              reqH <- getUrlHackage package
              httpSink reqH $ \resH -> do
                case () of
                  ()
                    | getResponseStatusCode resH `elem` [403, 404] -> do
                        mdiff <-
                          if getDiffs
                            then do
                                mlatest <- lift $ getLatestVersion $ packageName package
                                case mlatest of
                                  Nothing -> return Nothing
                                  Just (latest, latestv) -> do
                                    (isDiff, mdiff) <- lift $ compareTGZ getDiffs (packageName package) latest latestv fp (packageVersion package)
                                    return $ if isDiff then mdiff else Nothing
                            else return Nothing
                        return (DoesNotExist, mdiff)
                    | getResponseStatusCode resH == 403 -> return (DoesNotExist, Nothing)
                    | getResponseStatusCode resH == 200 -> do
                        createDirectoryIfMissing True $ takeDirectory localFileHackage
                        sinkFileCautious localFileHackage
                        lift handleFile
                    | otherwise -> error $ "Invalid status code: " ++ show (getResponseStatus resH)
    return $ Map.singleton status $ Map.singleton package mdiff

-- | Get the filepath for the latest version of a package from
-- Hackage, if it exists at all.
getLatestVersion :: PackageName -> RIO App (Maybe (FilePath, Version))
getLatestVersion name = do
    stack <- getAppUserDataDirectory "stack"
    let indexTar = stack </> "indices" </> "Hackage" </> "00-index.tar"
    mversion <- runConduitRes
        $ sourceFile indexTar
       .| untarChunks
       .| withEntries yield
       .| foldMapC (parseVersionNumber name)
    case mversion of
        Option Nothing -> return Nothing
        Option (Just (Max version)) -> do
            let p = Package "" name $ toTextVersion version
            fp <- getHackageFile p
            req <- getUrlHackage p
            httpSink req $ \res ->
              if getResponseStatusCode res == 200
                then do
                  createDirectoryIfMissing True $ takeDirectory fp
                  sinkFileCautious fp
                  return $ Just (fp, toTextVersion version)
                else error $ "Could not download from Hackage: " ++ show p

newtype PackageName = PackageName { unPackageName :: Text }
    deriving (Show, Eq, Ord)
newtype Version = Version { unVersion :: Text }
    deriving (Show, Eq, Ord)
data Package = Package
    { packageFile :: !FilePath
    , packageName :: !PackageName
    , packageVersion :: !Version
    }
    deriving (Show, Eq, Ord)

toTextVersion :: Data.Version.Version -> Version
toTextVersion = Version . T.pack . Data.Version.showVersion

parseVersionNumber :: PackageName
                   -- ^ target package we care about
                   -> Header
                   -> Option (Max Data.Version.Version)
parseVersionNumber pn header = Option $ fmap Max $ do
    [name, version, dotcabal] <- Just $ splitOn "/" $ T.pack fp
    guard $ PackageName name == pn
    guard $ name <> ".cabal" == dotcabal
    listToMaybe $ map fst
                $ filter (null . snd)
                $ readP_to_S Data.Version.parseVersion
                $ T.unpack version
  where
    fp = headerFilePath header

parsePackage :: MonadThrow m => FilePath -> m Package
parsePackage fp =
    case T.stripSuffix ".tar.gz" $ T.pack $ takeFileName fp of
        Nothing -> error $ "Does not end with .tar.gz: " ++ fp
        Just s -> do
            let s' = T.reverse s
                (b', a') = T.break (== '-') s'
                a = T.reverse $ T.drop 1 a'
                b = T.reverse b'
            return $ Package fp (PackageName a) (Version b)

sayPackage :: Package -> RIO App ()
sayPackage (Package _ (PackageName name) (Version version)) =
  logInfo $ display name <> "-" <> display version

getHackageFile :: MonadIO m => Package -> m FilePath
getHackageFile (Package _fp (PackageName a') (Version b')) = do
    stack <- getAppUserDataDirectory "stack"
    return $ stack </> "indices" </> "Hackage" </> "packages" </> a </> b </>
                concat [a, "-", b, ".tar.gz"]
  where
    a = T.unpack a'
    b = T.unpack b'

compareTGZ :: Bool -- ^ get diffs?
           -> PackageName
           -> FilePath
           -- ^ old tarball
           -> Version
           -- ^ old version
           -> FilePath
           -- ^ new tarball
           -> Version
           -- ^ new version
           -> RIO App (Bool, Maybe Diff)
compareTGZ getDiffs pn a av b bv = do
    a' <- getContents a
    b' <- getContents b
    let isDiff = a' /= b'
    mdiff <-
        if getDiffs && isDiff
            then withSystemTempDirectory "diff" $ \diff -> do
                let fill dir x = forM_ (Map.toList x) $ \(fp, bs) -> do
                      let fp' = dir </> fp
                      createDirectoryIfMissing True $ takeDirectory fp'
                      BL.writeFile fp' bs
                fill (diff </> "old") a'
                fill (diff </> "new") b'
                let toNV v = T.unpack $ mconcat
                        [ unPackageName pn
                        , "-"
                        , unVersion v
                        ]
                (_, out) <- withWorkingDir diff $ proc "diff"
                    [ "-ruN"
                    , "old" </> toNV av
                    , "new" </> toNV bv
                    ]
                    readProcessStdout
                return $ Just out
            else return Nothing
    return (a' /= b', mdiff)
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
