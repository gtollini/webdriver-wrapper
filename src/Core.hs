module Core (wrappedRunSession, wrappedRunWD) where

import System.Process (cleanupProcess)
import Control.Concurrent.Async (concurrently_)
import Selenium (getSeleniumIfNeeded, startSelenium)
import GeckoDriver (getGeckoDriverIfNeeded)
import Test.WebDriver (runSession)
import Test.WebDriver.Types (WD, WDSession)
import Test.WebDriver.Config (WebDriverConfig)
import Test.WebDriver.Monad (runWD)

wrappedRunSession :: WebDriverConfig conf => conf -> WD a -> IO a
wrappedRunSession conf wd = wrapWebDriverFunction (conf,wd) (uncurry runSession) 

wrappedRunWD ::  WDSession -> WD a -> IO a 
wrappedRunWD session wd = wrapWebDriverFunction (session, wd) (uncurry runWD)

wrapWebDriverFunction :: a -> (a -> IO b) -> IO b
wrapWebDriverFunction webdriverArgs webdriverFunction = do
    downloadIfMissing
    seleniumHandles <- startSelenium
    b <- webdriverFunction webdriverArgs
    cleanupProcess seleniumHandles
    return b

downloadIfMissing :: IO()
downloadIfMissing =  concurrently_ getSeleniumIfNeeded getGeckoDriverIfNeeded
