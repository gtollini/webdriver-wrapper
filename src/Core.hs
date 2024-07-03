{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Core (startSelenium, stopSelenium, killSelenium, downloadMissing, waitForSeleniumStart) where

import System.Directory (removeFile, doesFileExist)
import Constants (defaultPath, seleniumPath, geckoDriverPath, (</>))
import System.Process (createProcess, ProcessHandle, cleanupProcess)
import System.Process.Run (proc)
import GHC.IO.Handle (Handle, hGetLine)
import System.IO (openFile, IOMode (..))
import Control.Monad (unless)
import Data.List (isInfixOf)
import GHC.Base (when)
import Data.String.Interpolate (i)
import Control.Concurrent.Async (concurrently_)
import Selenium (getSeleniumIfNeeded)
import GeckoDriver (getGeckoDriverIfNeeded)
import Control.Retry (constantDelay, limitRetries, RetryStatus (..), retryOnError)
import System.IO.Error (isEOFError)
import Control.Monad.Extra (orM)

startSelenium :: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
startSelenium = do
    defaultPath' <- defaultPath
    writeFile (defaultPath' </> "seleniumLog.txt") ""
    selPath <- seleniumPath
    geckoPath <- geckoDriverPath
    
    let
        logFile = defaultPath' </> "seleniumLog.txt"
        selArgs = [[i|-Dwebdriver.gecko.driver=#{geckoPath}|], "-jar", selPath ,"-log", logFile]
        processParams = proc "java" selArgs

    processHandles <- createProcess processParams
    waitForSeleniumStart logFile
    return processHandles

downloadMissing :: IO()
downloadMissing =  concurrently_ getSeleniumIfNeeded getGeckoDriverIfNeeded

waitForSeleniumStart :: String -> IO()
waitForSeleniumStart logFile = do
    defaultPath' <- defaultPath
    fileHandle <-  openFile logFile ReadMode

    succeeded <- retryOnError  retryPolicy'
        (\_ e -> return $ isEOFError e)
        (\retryStatus -> if rsIterNumber retryStatus < 599 then logFileHasReadyMessage fileHandle else return False)

    unless succeeded $ error $ "Couldn't start Selenium successfully. Check " ++ (defaultPath' </> logFile)

        where
            -- wait a minute, retrying every 100ms
            retryPolicy' = constantDelay 100000 <> limitRetries 600

logFileHasReadyMessage :: Handle -> IO Bool
logFileHasReadyMessage fileHandle = orM remainingLines
    where
        remainingLines = repeat hasReadyMessage
        hasReadyMessage = (readyMessage `isInfixOf`) <$> hGetLine fileHandle

        readyMessage = "Selenium Server is up and running"

--
-- functions to help test Selenium and whatnot. Delete later.
--
stopSelenium :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO()
stopSelenium handles = do
    cleanupProcess handles
    defaultPath' <- defaultPath
    removeFile (defaultPath' </> "seleniumLog.txt")

killSelenium :: IO()
killSelenium = do
    defaultPath' <- defaultPath
    let
        seleniumLogFile = defaultPath' </> "seleniumLog.txt"
        in do
        _ <- createProcess $ proc "fuser" ["4444/tcp", "-k"]
        seleniumLogFileExists <- doesFileExist seleniumLogFile
        when seleniumLogFileExists $ removeFile seleniumLogFile
