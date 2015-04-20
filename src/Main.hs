{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.IORef
import System.FilePath (combine)
import Control.Monad (forM_, when)
import System.Environment (getArgs)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks)
import Network.Wai
import Network.HTTP.Types
import System.Directory
import Data.List (intercalate, sort)
import qualified Data.ByteString.Char8 as BSC
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.ByteString.Lazy as BSL

data AppState = AppState
                { pictureRoot :: !FilePath
                }

type Handler = ReaderT AppState IO

--newtype Generator a = Generator (IO (a, Maybe (Generator a)))

listDirectories' :: IORef [FilePath] -> FilePath -> IO ()
listDirectories' out root = do
    contents <- getDirectoryContents root
    forM_ contents $ \entry -> do
        let fe = combine root entry
        isdir <- doesDirectoryExist fe
        when (isdir && entry /= "." && entry /= "..") $ do
            modifyIORef out (fe:)
            listDirectories' out fe

listDirectories :: FilePath -> IO [FilePath]
listDirectories root = do
    rv <- newIORef []
    listDirectories' rv root
    out <- readIORef rv
    return $ sort $ reverse out

getFoldersR :: Handler Response
getFoldersR = do
    root <- asks pictureRoot
    directories <- lift $ listDirectories root
    
    return $ responseLBS status200 [] $ BSL.fromStrict $ BSC.intercalate "\n" $ map BSC.pack directories

app :: AppState -> Application
app state req respond = do
    if (requestMethod req /= "GET") then do
        respond $ responseLBS status404 [] "Method not supported"
    else do
        case pathInfo req of
            ["folders"] -> (runReaderT getFoldersR state) >>= respond
            _ -> respond $ responseLBS status404 [] "Not found"

readState :: IO AppState
readState = do
    [root] <- getArgs
    return $ AppState { pictureRoot = root }

main :: IO ()
main = do
    state <- readState
    Warp.run 9999 $ app state
