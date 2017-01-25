{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
import ClassyPrelude.Conduit
import System.Directory
import Network.HTTP.Simple
import qualified Codec.Archive.Tar as Tar
import Data.Conduit.Zlib (ungzip)
import System.Process.Typed
import System.FilePath
import Data.Conduit.Binary (sinkFileCautious)
import Data.Yaml (Value (..), decodeEither')

getUrlHackage :: Package -> IO Request
getUrlHackage (Package _fp (PackageName a) (Version b)) =
    parseRequest $ unpack $ concat
        [ "https://s3.amazonaws.com/hackage.fpcomplete.com/packages/archive/"
        , a
        , "/"
        , b
        , "/"
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

main :: IO ()
main = do
    args <- getArgs

    let toTag = "--gittag" `elem` args

    (queryBS, _) <- readProcess_ $ proc "stack" ["query"]
    queryValue <-
        case decodeEither' (toStrict queryBS) of
            Left e -> error $ "Could not parse 'stack query': " ++ show e
            Right x -> return x
    allDirs <- getPaths queryValue
    myPath <- canonicalizePath "."
    let dirs = filter (myPath `isPrefixOf`) allDirs

    whenM (doesDirectoryExist "tarballs") $ removeDirectoryRecursive "tarballs"
    createDirectoryIfMissing True "tarballs"

    tarballs <- forM dirs $ \dir -> do
        (_, output) <- readProcess_ $ setWorkingDir dir $ proc "stack" ["sdist"]
        case lastMay $ map unpack $ words $ decodeUtf8 output of
            Just fp -> do
                let dest = "tarballs" </> takeFileName fp
                renameFile fp dest
                return dest
            Nothing -> error $ "Unexpected 'stack sdist' output in dir: " ++ dir

    m <- unionsWith mappend <$> mapM go tarballs

    case lookup NoChanges m of
        Nothing -> return ()
        Just s -> do
            say "The following packages from Hackage have not changed:"
            mapM_ sayPackage s
            mapM_ (removeFile . packageFile) s

    case lookup DoesNotExist m of
        Nothing -> return ()
        Just s -> do
            say "\nThe following new packages exist locally:"
            mapM_ sayPackage s

    case lookup NeedsVersionBump m of
        Nothing -> do
            say "\nNo version bumps required, good to go!"
            when toTag $ do
                let pcs = fmap mkProcess
                         $ maybe [] toList $ lookup DoesNotExist m
                    mkProcess (Package _fp (PackageName name) (Version version)) =
                         proc "git" ["tag", unpack $ concat [name, "/", version]]
                mapM_ sayShow pcs
                mapM_ runProcess_ pcs
        Just s -> do
            say "\nThe following packages require a version bump:"
            mapM_ sayPackage s

data Status = DoesNotExist | NoChanges | NeedsVersionBump
    deriving (Show, Eq, Ord)

go :: FilePath -> IO (Map Status (Set Package))
go fp = do
    package <- parsePackage fp
    localFileHackage <- liftIO $ getHackageFile package
    fh <- liftIO $ doesFileExist localFileHackage
    let handleFile localFile noChanges = do
            isDiff <- compareTGZ localFile fp
            return $ if isDiff then NeedsVersionBump else noChanges
    status <-
        case () of
            ()
                | fh -> handleFile localFileHackage NoChanges
                | otherwise -> do
                    reqH <- getUrlHackage package
                    runResourceT $ httpSink reqH $ \resH -> do
                    case () of
                     ()
                      | getResponseStatusCode resH == 404 -> return DoesNotExist
                      | getResponseStatusCode resH == 403 -> return DoesNotExist
                      | getResponseStatusCode resH == 200 -> do
                            liftIO $ createDirectoryIfMissing True $ takeDirectory localFileHackage
                            sinkFileCautious localFileHackage
                            liftIO $ handleFile localFileHackage NoChanges
                      | otherwise -> error $ "Invalid status code: " ++ show (getResponseStatus resH)
    return $ singletonMap status $ singletonSet package

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

compareTGZ :: FilePath -> FilePath -> IO Bool
compareTGZ a b = do
    a' <- getContents a
    b' <- getContents b
    return $ a' /= b'
  where
    getContents :: FilePath -> IO (Map FilePath LByteString)
    getContents fp = do
        elbs <- tryAny
              $ runConduitRes
              $ sourceFile fp
             .| ungzip
             .| sinkLazy -- could use tar-conduit and avoid in-memory, but it'll happen later anyway
        case elbs of
            Left e -> do
                say $ concat
                    [ "Error opening tarball: "
                    , pack fp
                    , ", "
                    , tshow e
                    ]
                return mempty
            Right lbs -> foldMap go' <$> toList' (Tar.read lbs)
    toList' (Tar.Next e es) = do
        l <- toList' es
        return $ e : l
    toList' Tar.Done = return []
    toList' (Tar.Fail s) = error $ show s
    go' e =
        case Tar.entryContent e of
            Tar.NormalFile lbs _ -> asMap $ singletonMap (Tar.entryPath e) lbs
            _ -> mempty
