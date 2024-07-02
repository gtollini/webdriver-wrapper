{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Core (startSelenium, stopSelenium, killSelenium, downloadMissing, testFunction) where

import System.Directory (createDirectoryIfMissing, removeFile, doesFileExist)
import Constants (desiredPlatform, defaultPath, seleniumPath, geckoDriverPath, (</>))
import System.Process (createProcess, ProcessHandle, cleanupProcess, createProcess_)
import System.Process.Run (proc, CreateProcess (..))
import GHC.IO.Handle (Handle, hPutChar, hPutStr)
import System.Process.Internals (StdStream(..))
import System.IO (openFile, IOMode (..))
import Control.Monad (unless)
import Data.List (isInfixOf)
import GHC.Base (when)
import Data.String.Interpolate (i)
import Control.Concurrent.Async (concurrently_)
import Selenium (getSeleniumIfNeeded)
import GeckoDriver (getGeckoDriverIfNeeded)

-- I think it always outputs the errors at the file, which might cause some hidden problems.
-- TODO: Check for alternatives.
startSelenium :: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
startSelenium = do
    selPath <- seleniumPath
    geckoPath <- geckoDriverPath
    defaultPath' <- defaultPath
    let 
        selArgs = [[i|-Dwebdriver.gecko.driver=#{geckoPath}|], "-jar", selPath ,"-log", defaultPath' </> "hout"]
        processParams = proc "java" selArgs
    print $ defaultPath' </> "hout"
    createProcess processParams

downloadMissing :: IO()
downloadMissing =  concurrently_ getSeleniumIfNeeded getGeckoDriverIfNeeded

--
-- functions to help test Selenium and whatnot. Delete later.
--
stopSelenium :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO()
stopSelenium handles = do
    cleanupProcess handles
    defaultPath' <- defaultPath
    removeFile (defaultPath' ++ "/hout")
    removeFile (defaultPath' ++ "/herr")

killSelenium :: IO()
killSelenium = do
    defaultPath' <- defaultPath
    let
        herr = defaultPath' ++ "/herr"
        hout = defaultPath' ++ "/hout"
        in do
        _ <- createProcess $ proc "fuser" ["4444/tcp", "-k"]
        herrExists <- doesFileExist herr 
        houtExists <- doesFileExist hout
        when herrExists $ removeFile herr
        when houtExists $ removeFile hout

fullWait :: IO()
fullWait = do
    defaultPath' <- defaultPath
    content1 <- readFile (defaultPath' ++ "/hout")
    content2 <- readFile (defaultPath' ++ "/herr")
    unless (readyMessage `isInfixOf` content1 || readyMessage `isInfixOf` content2) fullWait
    where
        readyMessage = "Selenium Server is up and running"
