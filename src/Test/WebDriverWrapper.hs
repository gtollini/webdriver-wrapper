{- |
Module : Core
Description : end-user functions.

The wrapped functions (`wrappedRunSession` and `wrappedRunWD`) will download Selenium and Firefox's webdriver (geckodriver) if they're not already on the `Test.WebDriverWrapper.Constants.downloadPath`, then start Selenium before running the webdriver equivalent function (`runSession` and `runWD`). They kill the Selenium process at the end of their execution. 
-}
module Test.WebDriverWrapper (wrappedRunSession, wrappedRunWD, wrapWebDriverFunction, downloadIfMissing) where

import System.Process (cleanupProcess)
import Control.Concurrent.Async (concurrently_)
import Test.WebDriverWrapper.Selenium (getSeleniumIfNeeded, startSelenium)
import Test.WebDriverWrapper.GeckoDriver (getGeckoDriverIfNeeded)
import Test.WebDriver (runSession)
import Test.WebDriver.Types (WD, WDSession)
import Test.WebDriver.Config (WebDriverConfig)
import Test.WebDriver.Monad (runWD)

-- | Same as `runSession`, but starts Selenium before execution and kills Selenium after execution. 
-- Will download Selenium or Firefox's webdriver (geckodriver) if any is missing. 
wrappedRunSession :: WebDriverConfig conf => conf -> WD a -> IO a
wrappedRunSession conf wd = wrapWebDriverFunction (conf,wd) (uncurry runSession) 

-- | Same as `runWD`, but starts Selenium before execution and kills Selenium after execution. 
-- Will download Selenium or Firefox's webdriver (geckodriver) if any is missing. 
wrappedRunWD ::  WDSession -> WD a -> IO a 
wrappedRunWD session wd = wrapWebDriverFunction (session, wd) (uncurry runWD)

-- | Runs a function in between starting and killing Selenium. Takes in the arguments and the function, in that order. 
-- Will download Selenium or Firefox's webdriver (geckodriver) if any is missing. 
wrapWebDriverFunction :: a -> (a -> IO b) -> IO b
wrapWebDriverFunction webdriverArgs webdriverFunction = do
    downloadIfMissing
    seleniumHandles <- startSelenium
    b <- webdriverFunction webdriverArgs
    cleanupProcess seleniumHandles
    return b

-- | Dowloads Selenium or Firefox's webdriver (geckodriver) if they're missing. 
downloadIfMissing :: IO()
downloadIfMissing =  concurrently_ getSeleniumIfNeeded getGeckoDriverIfNeeded
