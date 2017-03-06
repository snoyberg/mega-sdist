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
import Options.Applicative.Simple
import Paths_mega_sdist (version)
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.ByteString.Lazy as L

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
        $(simpleVersion version)
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

    case lookup DoesNotExist m of
        Nothing -> return ()
        Just s -> do
            say "\nThe following new packages exist locally:"
            mapM_ sayPackage $ keys s

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
                forM_ mdiff (L.hPut stdout)

data Status = DoesNotExist | NoChanges | NeedsVersionBump
    deriving (Show, Eq, Ord)

type Diff = LByteString

go :: Bool -- ^ get diffs
   -> FilePath
   -> IO (Map Status (Map Package (Maybe Diff)))
go getDiffs fp = do
    package <- parsePackage fp
    localFileHackage <- liftIO $ getHackageFile package
    fh <- liftIO $ doesFileExist localFileHackage
    let handleFile localFile noChanges = do
            (isDiff, mdiff) <- compareTGZ getDiffs localFile fp
            return $ if isDiff then (NeedsVersionBump, mdiff) else (noChanges, Nothing)
    (status, mdiff) <-
        case () of
            ()
                | fh -> handleFile localFileHackage NoChanges
                | otherwise -> do
                    reqH <- getUrlHackage package
                    runResourceT $ httpSink reqH $ \resH -> do
                    case () of
                     ()
                      | getResponseStatusCode resH == 404 -> return (DoesNotExist, Nothing)
                      | getResponseStatusCode resH == 403 -> return (DoesNotExist, Nothing)
                      | getResponseStatusCode resH == 200 -> do
                            liftIO $ createDirectoryIfMissing True $ takeDirectory localFileHackage
                            sinkFileCautious localFileHackage
                            liftIO $ handleFile localFileHackage NoChanges
                      | otherwise -> error $ "Invalid status code: " ++ show (getResponseStatus resH)
    return $ singletonMap status $ singletonMap package mdiff

newtype PackageName = PackageName Text
    deriving (Show, Eq, Ord)
newtype Version = Version Text
    deriving (Show, Eq, Ord)
data Package = Package
    { packageFile :: !FilePath
    , _packageName :: !PackageName
    , _packageVersion :: !Version
    }
    deriving (Show, Eq, Ord)

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
           -> FilePath -> FilePath -> IO (Bool, Maybe Diff)
compareTGZ getDiffs a b = do
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
                (_, out, _) <- readProcess $ setWorkingDir diff $ proc "diff" ["-r", "old", "new"]
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
