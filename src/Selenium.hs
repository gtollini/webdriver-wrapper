module Selenium (getSelenium, getSeleniumIfNeeded) where
    
import Constants (defaultSeleniumJarUrl, seleniumPath, downloadPath)
import Helpers (download)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Control.Monad (unless)
    
getSeleniumIfNeeded :: IO ()
getSeleniumIfNeeded = do
    selPath <- seleniumPath
    hasSelenium  <- doesFileExist selPath
    unless hasSelenium getSelenium


getSelenium :: IO ()
getSelenium = do
    dPath <- downloadPath
    sPath <- seleniumPath
    
    createDirectoryIfMissing True dPath

    download defaultSeleniumJarUrl sPath