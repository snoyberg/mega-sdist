{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Prelude hiding (FilePath, getContents)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist)
import Network.HTTP.Conduit
import Network.HTTP.Types (status200, status404, status502)
import Filesystem
import Filesystem.Path.CurrentOS hiding (concat)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (when, forM_, forM, filterM)
import qualified Data.ByteString.Lazy as L
import qualified Codec.Archive.Tar as Tar
import Data.Conduit.Zlib (ungzip)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Exception (try, SomeException (..))
import Control.Monad.IO.Class (liftIO)
import Shelly hiding ((</>))
import Data.Maybe (mapMaybe, fromMaybe)
import Network (withSocketsDo)

debug :: String -> IO ()
#ifdef DEBUG
debug = putStrLn
#else
debug = const $ return ()
#endif

getUrlHackage :: Package
#if MIN_VERSION_http_conduit(2, 0, 0)
              -> IO Request
#else
              -> IO (Request m)
#endif
getUrlHackage (Package a b) = do
    debug url
    req <- parseUrl url
    return req { responseTimeout = Nothing }
  where
    url = concat
        [ "http://hackage.haskell.org/packages/archive/"
        , a
        , "/"
        , b
        , "/"
        , a
        , "-"
        , b
        , ".tar.gz"
        ]

main :: IO ()
main = withSocketsDo $ do
    manager <- newManager
#if MIN_VERSION_http_conduit(2, 0 ,0)
        conduitManagerSettings
#else
        def
#endif
    args <- getArgs

    let toTest = "--test" `elem` args
        toTag = "--gittag" `elem` args

    exists <- isFile "sources.txt"
    dirs <-
        if exists
            then fmap lines (Prelude.readFile "sources.txt") >>= filterM doesDirectoryExist
            else return ["."]
    shelly $ do
        rm_rf "tarballs"
        mkdir "tarballs"
        files' <- forM dirs $ \dir -> do
            chdir (decodeString dir) $ do
                rm_rf "dist"
                when toTest $ do
                    run_ "cabal" ["configure", "--enable-tests", "-ftest_export"]
                    run_ "cabal" ["build"]
                    run_ "cabal" ["test"]
                run_ "cabal" ["sdist"]
                ls "dist" >>= mapM absPath . filter (flip hasExtension "gz")
        forM_ (concat files') $ \file -> mv file $ "tarballs" </> filename file

    tarballs <- listDirectory "tarballs"
    ss <- mapM (go manager) tarballs
    let m = Map.unionsWith Set.union ss
    let say = putStrLn . reverse . drop 7 . reverse . encodeString . filename

    case Map.lookup NoChanges m of
        Nothing -> return ()
        Just s -> do
            putStrLn "The following packages from Hackage have not changed:"
            mapM_ say $ Set.toList s
            mapM_ removeFile $ Set.toList s

    case Map.lookup DoesNotExist m of
        Nothing -> return ()
        Just s -> do
            putStrLn "\nThe following new packages exist locally:"
            mapM_ say $ Set.toList s

    case Map.lookup NeedsVersionBump m of
        Nothing -> do
            putStrLn "\nNo version bumps required, good to go!"
            when toTag $ do
                let tags = mapMaybe (mkTag . either id id . toText . filename) $ Set.toList $ fromMaybe Set.empty $ Map.lookup DoesNotExist m
                    mkTag t = do
                        base <- T.stripSuffix ".tar.gz" t
                        let (x', y) = T.breakOnEnd "-" base
                        x <- T.stripSuffix "-" x'
                        return $ T.concat [x, "/", y]
                forM_ tags $ \tag -> putStrLn $ "git tag " ++ T.unpack tag
                shelly $ forM_ tags $ \tag -> run_ "git" ["tag", tag]
        Just s -> do
            putStrLn "\nThe following packages require a version bump:"
            mapM_ say $ Set.toList s

data Status = DoesNotExist | NoChanges | NeedsVersionBump
    deriving (Show, Eq, Ord)

go :: Manager -> FilePath -> IO (Map.Map Status (Set.Set FilePath))
go m fp = do
    let base = T.reverse $ T.drop 7 $ T.reverse $ either id id $ toText $ filename fp
    let package = parsePackage $ T.unpack base
    localFileHackage <- liftIO $ getHackageFile package
    fh <- liftIO $ isFile localFileHackage
    let handleFile localFile noChanges = do
            debug $ "Comparing: " ++ show (fp, localFile)
            isDiff <- compareTGZ localFile fp
            return $ if isDiff then NeedsVersionBump else noChanges
    status <-
        case () of
            ()
                | fh -> handleFile localFileHackage NoChanges
                | otherwise -> do
                    reqH <- getUrlHackage package
                    resH <- runResourceT $ httpLbs reqH
#if MIN_VERSION_http_conduit(1, 9, 0)
                        { checkStatus = \_ _ _ -> Nothing
#else
                        { checkStatus = \_ _ -> Nothing
#endif
                        } m
                    case () of
                        ()
                            | responseStatus resH == status404 || L.length (responseBody resH) == 0 -> do
                                liftIO $ debug $ "Not found on Hackage: " ++ show fp
                                return DoesNotExist
                            | responseStatus resH == status200 -> do
                                createTree $ directory localFileHackage
                                L.writeFile (encodeString localFileHackage) $ responseBody resH
                                handleFile localFileHackage NoChanges
                            | otherwise -> error $ "Invalid status code: " ++ show (responseStatus resH)
    return $ Map.singleton status $ Set.singleton fp

data Package = Package String String

parsePackage :: String -> Package
parsePackage s =
    Package a b
  where
    s' = reverse s
    (b', a') = break (== '-') s'
    a = reverse $ drop 1 a'
    b = reverse b'

getHackageFile :: Package -> IO FilePath
getHackageFile (Package a b) = do
    cache <- getAppCacheDirectory "sdist-check"
    return $ cache </> "hackage" </> decodeString (concat [a, "-", b, ".tar.gz"])

compareTGZ :: FilePath -> FilePath -> IO Bool
compareTGZ a b = {- FIXME catcher $ -} do
    a' <- getContents a
    b' <- getContents b
    return $ a' /= b'
  where
    -- catcher = handle (\SomeException{} -> debug (show ("compareTGZ" :: String, a, b)) >> return True)
    getContents fp = do
        lbs <- L.readFile (encodeString fp)
        ebss <- try $ runResourceT $ CL.sourceList (L.toChunks lbs) C.$$ ungzip C.=$ CL.consume
        case ebss of
            Left (e :: SomeException) -> do
                putStrLn $ concat
                    [ "Error opening tarball: "
                    , encodeString fp
                    , ", "
                    , show e
                    ]
                return Map.empty
            Right bss -> do
                l <- toList $ Tar.read $ L.fromChunks bss
                return $ Map.unions $ map go' l
    toList (Tar.Next e es) = do
        l <- toList es
        return $ e : l
    toList Tar.Done = return []
    toList (Tar.Fail s) = error $ show s
    go' e =
        case Tar.entryContent e of
            Tar.NormalFile lbs _ -> Map.singleton (Tar.entryPath e) lbs
            _ -> Map.empty
