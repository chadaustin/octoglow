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
    rv <- newIORef []
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
            return $ isFile && (map toLower (takeExtension p) `elem` [".jpeg", ".jpg"])
    
    filterM isJPEG contents

jsonResponse :: Json.Value -> Response
jsonResponse v =
    responseLBS status200 [] $ Json.encode v

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
            return $ jsonResponse $ Json.object ["pictures" .= contents]
        Nothing -> notFound

route :: Handler Response
route = do
    req <- asks asRequest
    case pathInfo req of
        ["folders"] -> getFoldersR
        ["contents"] -> getContentsR
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
