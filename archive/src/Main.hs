{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (filterM, forM_, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks)
import Data.Aeson ((.=))
import Data.Char (toLower)
import Data.IORef
import Data.List (intercalate, sort)
import Debug.Trace (traceM)
import Network.HTTP.Types
import Network.Wai
import System.Directory
import System.Environment (getArgs)
import System.FilePath (combine, takeExtension)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Network.Wai.Handler.Warp as Warp

data AppState = AppState
                { asPictureRoot :: !FilePath
                , asRequest :: !Request
                }

type Handler = ReaderT AppState IO

listDirectories' :: IORef [FilePath] -> FilePath -> FilePath -> IO ()
listDirectories' out root prefix = do
    contents <- getDirectoryContents root
    forM_ contents $ \entry -> do
        let fe = combine root entry
        isdir <- doesDirectoryExist fe
        when (isdir && entry /= "." && entry /= "..") $ do
            let oe = combine prefix entry
            modifyIORef out (oe:)
            listDirectories' out fe oe

listDirectories :: FilePath -> FilePath -> IO [FilePath]
listDirectories root prefix = do
    rv <- newIORef ["."]
    listDirectories' rv root prefix
    out <- readIORef rv
    return $ sort $ reverse out

readFolderContents :: FilePath -> FilePath -> IO [FilePath]
readFolderContents root prefix = do
    let fp = combine root prefix
    contents <- getDirectoryContents fp

    let isJPEG :: FilePath -> IO Bool
        isJPEG p = do
            isFile <- doesFileExist $ combine fp p
            let endsWithJPEG = map toLower (takeExtension p) `elem` [".jpeg", ".jpg"]
                stupid = List.isPrefixOf "._" p
            return $ isFile && endsWithJPEG && not stupid
    
    filterM isJPEG contents

jsonResponse :: Json.Value -> Response
jsonResponse v =
    responseLBS status200 [("Access-Control-Allow-Origin", "*")] $ Json.encode v

getFoldersR :: Handler Response
getFoldersR = do
    root <- asks asPictureRoot
    directories <- lift $ listDirectories root ""

    return $ jsonResponse $ Json.object [("folders" .= directories)]

notFound :: Handler Response
notFound = do
    return $ responseLBS status404 [] "Not found"

getContentsR :: Handler Response
getContentsR = do
    root <- asks asPictureRoot
    queries <- fmap queryString $ asks asRequest
    let mfolder = lookup "folder" queries
    case mfolder of
        Just (Just folder) -> do
            contents <- lift $ readFolderContents root (BSC.unpack folder)
            let objects = [Json.object
                           [ "folder" .= BSC.unpack folder
                           , "name" .= f
                           ] | f <- contents]
            return $ jsonResponse $ Json.object ["pictures" .= objects]
        Nothing -> notFound

getPhotoR :: Handler Response
getPhotoR = do
    root <- asks asPictureRoot
    queries <- fmap queryString $ asks asRequest
    let mfolder = lookup "folder" queries
    let mphoto = lookup "photo" queries
    case (mfolder, mphoto) of
        (Just (Just folder), Just (Just photo)) -> do
            contents <- lift $ BSL.readFile $ combine root (combine (BSC.unpack folder) (BSC.unpack photo))
            return $ responseLBS status200 [("Content-Type", "image/jpeg")] contents
        _ -> notFound

getIndexR :: Handler Response
getIndexR = getFrontendAssetR "index.html"

getFrontendAssetR :: Text.Text -> Handler Response
getFrontendAssetR name = do
    let fullPath = combine "frontend" (Text.unpack name)
    exists <- lift $ doesFileExist $ fullPath
    if exists then do
        contents <- lift $ BSL.readFile fullPath
        let contentType = case takeExtension (Text.unpack name) of
                ".html" -> Just "text/html"
                ".css" -> Just "text/css"
                ".js" -> Just "text/javascript"
                _ -> Nothing
        let headers = case contentType of
                Just ct -> [("Content-Type", ct)]
                Nothing -> []
        return $ responseLBS status200 headers contents
    else
        notFound
                    

route :: Handler Response
route = do
    req <- asks asRequest
    lift $ putStrLn $ Text.unpack $ Text.intercalate "/" $ pathInfo req
    case pathInfo req of
        -- services
        ["folders"] -> getFoldersR
        ["contents"] -> getContentsR
        ["photo"] -> getPhotoR

        -- frontend
        [] -> getIndexR
        [asset] -> getFrontendAssetR asset
        _ -> notFound

app :: FilePath -> Application
app proot req respond = do
    if (requestMethod req /= "GET") then do
        respond $ responseLBS status405 [] "Method not supported"
    else do
        let state = AppState { asPictureRoot = proot, asRequest = req }
        response <- runReaderT route state
        respond response

readArgs :: IO FilePath
readArgs = do
    [root] <- getArgs
    return $ root

main :: IO ()
main = do
    proot <- readArgs
    Warp.run 9999 $ app proot
