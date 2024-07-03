{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Selenium (startSelenium, getSeleniumIfNeeded) where

import Constants (defaultSeleniumJarUrl, seleniumPath, downloadPath, geckoDriverPath, seleniumLogPath)
import Helpers (download)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Control.Monad (unless)
import GHC.IO.Handle ( Handle, hGetLine )
import System.Process (ProcessHandle, createProcess)
import Data.String.Interpolate (i)
import System.IO (openFile)
import GHC.IO.IOMode (IOMode(..))
import Control.Retry (retryOnError, RetryStatus (..))
import System.IO.Error (isEOFError)
import UnliftIO.Retry ( constantDelay, limitRetries )
import Data.Foldable.Extra (orM)
import Data.List (isInfixOf)
import System.Process.Run (proc)

startSelenium :: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
startSelenium = do
    geckoPath <- geckoDriverPath
    selPath <- seleniumPath
    logFile <- seleniumLogPath
    writeFile logFile "" -- Create file if it doesn't exist, clears it if it does. Needs to happen for "logFileHasReadyMessage" to work correctly. 
    let
        selArgs = [[i|-Dwebdriver.gecko.driver=#{geckoPath}|], "-jar", selPath ,"-log", logFile]
        processParams = proc "java" selArgs
    processHandles <- createProcess processParams
    waitForSeleniumStart
    return processHandles

waitForSeleniumStart :: IO()
waitForSeleniumStart = do
    logFile <- seleniumLogPath
    fileHandle <-  openFile logFile ReadMode
    succeeded <- retryOnError  retryPolicy'
        (\_ e -> return $ isEOFError e)
        (\retryStatus -> if rsIterNumber retryStatus < 599 then logFileHasReadyMessage fileHandle else return False)
    unless succeeded $ error $ "Couldn't start Selenium successfully. Check " <> logFile <> " for more information."
        where
            -- waits a minute, retrying every 100ms
            retryPolicy' = constantDelay 100000 <> limitRetries 600

logFileHasReadyMessage :: Handle -> IO Bool
logFileHasReadyMessage fileHandle = orM remainingLines
    where
        remainingLines = repeat hasReadyMessage
        hasReadyMessage = (readyMessage `isInfixOf`) <$> hGetLine fileHandle

        readyMessage = "Selenium Server is up and running"

getSeleniumIfNeeded :: IO ()
getSeleniumIfNeeded = do
    selPath <- seleniumPath
    hasSelenium  <- doesFileExist selPath
    unless hasSelenium getSelenium

getSelenium :: IO ()
getSelenium = do
    downloadPath' <- downloadPath
    selPath       <- seleniumPath
    createDirectoryIfMissing True downloadPath'
    download defaultSeleniumJarUrl selPath
