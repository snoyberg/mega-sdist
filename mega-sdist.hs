{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
import ClassyPrelude.Conduit
import System.Directory
import Network.HTTP.Simple
import Data.Conduit.Tar
import Data.Conduit.Zlib (ungzip)
import System.Process.Typed
import System.FilePath
import Data.Conduit.Binary (sinkFileCautious)
import Data.Yaml (Value (..), decodeEither')
import Options.Applicative.Simple hiding (header, value)
import qualified Paths_mega_sdist as Paths (version)
import System.IO (hIsTerminalDevice)
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.ByteString.Lazy as L
import Data.Semigroup (Max (..), Option (..))
import qualified Data.Version
import Text.ParserCombinators.ReadP (readP_to_S)
import qualified Data.Text as T

getUrlHackage :: Package -> IO Request
getUrlHackage (Package _fp (PackageName a) (Version b)) =
    parseRequest $ unpack $ concat
        [ "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"
        , a
        , "-"
        , b
        , ".tar.gz"
        ]

getPaths :: Value -> IO [FilePath]
getPaths value = maybe (error $ "getPaths failed: " ++ show value) return $ do
    Object top <- return value
    Object locals <- lookup "locals" top
    forM (toList locals) $ \l -> do
        Object o <- return l
        String path <- lookup "path" o
        return $ unpack path

data Args = Args
    { toTag :: !Bool
    , getDiffs :: !Bool
    , rawDirs :: ![FilePath]
    }

main :: IO ()
main = do
    (Args {..}, ()) <- simpleOptions
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
        )
        empty

    (queryBS, _) <- readProcess_ $ proc "stack" ["query"]
    queryValue <-
        case decodeEither' (toStrict queryBS) of
            Left e -> error $ "Could not parse 'stack query': " ++ show e
            Right x -> return x
    allDirs <- getPaths queryValue
    dirs <-
      if null rawDirs
        then return allDirs
        else do
          myPaths <- mapM (fmap addTrailingPathSeparator . canonicalizePath) rawDirs
          return $ filter (\y -> any (\x -> (x `isPrefixOf` y)) myPaths) (map addTrailingPathSeparator allDirs)

    whenM (doesDirectoryExist "tarballs") $ removeDirectoryRecursive "tarballs"
    createDirectoryIfMissing True "tarballs"

    tarballs <- forM dirs $ \dir -> do
        (_, output) <- readProcess_ $ proc "stack" ["sdist", dir]
        case lastMay $ map unpack $ words $ decodeUtf8 output of
            Just fp -> do
                let dest = "tarballs" </> takeFileName fp
                renameFile fp dest
                return dest
            Nothing -> error $ "Unexpected 'stack sdist' output in dir: " ++ dir

    m <- unionsWith mappend <$> mapM (go getDiffs) tarballs

    case lookup NoChanges m of
        Nothing -> return ()
        Just s -> do
            say "The following packages from Hackage have not changed:"
            mapM_ sayPackage $ keys s
            mapM_ (removeFile . packageFile) $ keys s

    toColor <- hIsTerminalDevice stdout

    case lookup DoesNotExist m of
        Nothing -> return ()
        Just s -> do
            say "\nThe following new packages exist locally:"
            forM_ (mapToList s) $ \(name, mdiff) -> do
                sayPackage name
                sayDiff toColor mdiff

    case lookup NeedsVersionBump m of
        Nothing -> do
            say "\nNo version bumps required, good to go!"
            when toTag $ do
                let pcs = fmap mkProcess
                         $ maybe [] keys $ lookup DoesNotExist m
                    mkProcess (Package _fp (PackageName name) (Version version)) =
                         proc "git" ["tag", unpack $ concat [name, "/", version]]
                mapM_ sayShow pcs
                mapM_ runProcess_ pcs
        Just s -> do
            say "\nThe following packages require a version bump:"
            forM_ (mapToList s) $ \(name, mdiff) -> do
                sayPackage name
                sayDiff toColor mdiff

sayDiff :: Bool -- ^ use color?
        -> Maybe Diff -> IO ()
sayDiff toColor = mapM_ $ L.hPut stdout . (if toColor then colorize else id)

data Status = DoesNotExist | NoChanges | NeedsVersionBump
    deriving (Show, Eq, Ord)

type Diff = LByteString

go :: Bool -- ^ get diffs
   -> FilePath
   -> IO (Map Status (Map Package (Maybe Diff)))
go getDiffs fp = do
    package <- parsePackage fp
    localFileHackage <- liftIO $ getHackageFile package
    localFileExists <- liftIO $ doesFileExist localFileHackage
    let handleFile = do
            let v = packageVersion package
            (isDiff, mdiff) <- compareTGZ getDiffs (packageName package) localFileHackage v fp v
            return $ if isDiff then (NeedsVersionBump, mdiff) else (NoChanges, Nothing)
    (status, mdiff) <-
        if localFileExists
            then handleFile
            else do
              reqH <- getUrlHackage package
              runResourceT $ httpSink reqH $ \resH -> do
                case () of
                  ()
                    | getResponseStatusCode resH `elem` [403, 404] -> do
                        mdiff <-
                          if getDiffs
                            then do
                                mlatest <- getLatestVersion (packageName package)
                                case mlatest of
                                  Nothing -> return Nothing
                                  Just (latest, latestv) -> do
                                    (isDiff, mdiff) <- liftIO $ compareTGZ getDiffs (packageName package) latest latestv fp (packageVersion package)
                                    return $ if isDiff then mdiff else Nothing
                            else return Nothing
                        return (DoesNotExist, mdiff)
                    | getResponseStatusCode resH == 403 -> return (DoesNotExist, Nothing)
                    | getResponseStatusCode resH == 200 -> do
                        liftIO $ createDirectoryIfMissing True $ takeDirectory localFileHackage
                        sinkFileCautious localFileHackage
                        liftIO handleFile
                    | otherwise -> error $ "Invalid status code: " ++ show (getResponseStatus resH)
    return $ singletonMap status $ singletonMap package mdiff

-- | Get the filepath for the latest version of a package from
-- Hackage, if it exists at all.
getLatestVersion :: MonadIO m => PackageName -> m (Maybe (FilePath, Version))
getLatestVersion name = liftIO $ do
    stack <- getAppUserDataDirectory "stack"
    let indexTar = stack </> "indices" </> "Hackage" </> "00-index.tar"
    mversion <- runConduitRes
        $ sourceFile indexTar
       .| untar
       .| withEntries yield
       .| foldMapC (parseVersionNumber name)
    case mversion of
        Option Nothing -> return Nothing
        Option (Just (Max version)) -> do
            let p = Package "" name $ toTextVersion version
            fp <- liftIO $ getHackageFile p
            req <- liftIO $ getUrlHackage p
            runResourceT $ httpSink req $ \res ->
              if getResponseStatusCode res == 200
                then do
                  liftIO $ createDirectoryIfMissing True $ takeDirectory fp
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
toTextVersion = Version . pack . Data.Version.showVersion

parseVersionNumber :: PackageName
                   -- ^ target package we care about
                   -> Header
                   -> Option (Max Data.Version.Version)
parseVersionNumber pn header = Option $ fmap Max $ do
    [name, version, dotcabal] <- Just $ T.splitOn "/" $ pack fp
    guard $ PackageName name == pn
    guard $ name ++ ".cabal" == dotcabal
    listToMaybe $ map fst
                $ filter (null . snd)
                $ readP_to_S Data.Version.parseVersion
                $ unpack version
  where
    fp = headerFilePath header

parsePackage :: MonadThrow m => FilePath -> m Package
parsePackage fp =
    case stripSuffix ".tar.gz" $ pack $ takeFileName fp of
        Nothing -> error $ "Does not end with .tar.gz: " ++ unpack fp
        Just s -> do
            let s' = reverse s
                (b', a') = break (== '-') s'
                a = reverse $ drop 1 a'
                b = reverse b'
            return $ Package fp (PackageName a) (Version b)

sayPackage :: MonadIO m => Package -> m ()
sayPackage (Package _ (PackageName name) (Version version)) = say $ concat [name, "-", version]

getHackageFile :: Package -> IO FilePath
getHackageFile (Package _fp (PackageName a') (Version b')) = do
    stack <- getAppUserDataDirectory "stack"
    return $ stack </> "indices" </> "Hackage" </> "packages" </> a </> b </>
                concat [a, "-", b, ".tar.gz"]
  where
    a = unpack a'
    b = unpack b'

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
           -> IO (Bool, Maybe Diff)
compareTGZ getDiffs pn a av b bv = do
    a' <- getContents a
    b' <- getContents b
    let isDiff = a' /= b'
    mdiff <-
        if getDiffs && isDiff
            then withSystemTempDirectory "diff" $ \diff -> do
                let fill dir x = forM_ (mapToList (asMap x)) $ \(fp, bs) -> do
                      let fp' = dir </> fp
                      createDirectoryIfMissing True $ takeDirectory fp'
                      L.writeFile fp' bs
                fill (diff </> "old") a'
                fill (diff </> "new") b'
                let toNV v = concat
                        [ unpack (unPackageName pn)
                        , "-"
                        , unpack (unVersion v)
                        ]
                (_, out, _) <- readProcess $ setWorkingDir diff $ proc "diff"
                    [ "-ru"
                    , "old" </> toNV av
                    , "new" </> toNV bv
                    ]
                return $ Just out
            else return Nothing
    return (a' /= b', mdiff)
  where
    getContents :: FilePath -> IO (Map FilePath LByteString)
    getContents fp = handleAny (onErr fp) $ runConduitRes
         $ sourceFile fp
        .| ungzip
        .| untar
        .| withEntries addEntry
        .| foldC

    onErr fp e = do
        say $ concat
            [ "Error opening tarball: "
            , pack fp
            , ", "
            , tshow e
            ]
        return mempty

    addEntry header
        | headerFileType header == FTNormal = do
            lbs <- sinkLazy
            yield $ asMap $ singletonMap (headerFilePath header) lbs
        | otherwise = return ()

colorize :: LByteString -> LByteString
colorize =
    intercalate "\n" . map colorLine . L.split 10
  where
    colorLine :: LByteString -> LByteString
    colorLine l =
      case (toEnum . fromEnum) <$> headMay l of
        Just '-' -> add "31" l
        Just '+' -> add "32" l
        Just '@' -> add "34" l
        _ -> l

    add :: LByteString -> LByteString -> LByteString
    add color l = concat
      [ "\x1b["
      , color
      , "m"
      , l
      , "\x1b[0m"
      ]
