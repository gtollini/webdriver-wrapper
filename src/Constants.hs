{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module : Constants
Description : Constant values, such as links and paths.
 -}

module Constants (geckoDriverPath, defaultPath, defaultSeleniumJarUrl, desiredPlatform, getGeckoDriverDownloadUrl, geckoDriverVersionSource, downloadPath, geckoArchivePath, fileFormat, seleniumPath, seleniumLogPath) where

import Data.String.Interpolate (i)
import qualified System.Info as SI
import System.Directory (getXdgDirectory, XdgDirectory (XdgData))
import System.FilePath ((</>))

-- Paths. Should be platform independent.
-- | @haskell-webdriver-wrapper@ directory, created at `XdgData`. 
defaultPath :: IO FilePath
defaultPath = getXdgDirectory XdgData "haskell-webdriver-wrapper"

-- | Directory named after `desiredPlatform`, created at the `defaultPath`.
downloadPath :: IO FilePath
downloadPath = (</> desiredPlatform) <$> defaultPath

-- | Intermediary path for the compressed version of geckodriver. Inside `downloadPath`.
geckoArchivePath :: IO FilePath
geckoArchivePath = (</> archive) <$> downloadPath
    where
        archive = "geckodriver" <> fileFormat

-- | Path for geckodriver. Inside `downloadPath`. 
geckoDriverPath :: IO FilePath
geckoDriverPath = (</> "geckodriver") <$> downloadPath

-- | Path for selenium.jar. Inside `downloadPath`.
seleniumPath :: IO FilePath
seleniumPath = (</> "selenium.jar") <$> downloadPath

-- | Path for Selenium's log file. Inside `defaultPath`. 
seleniumLogPath :: IO FilePath
seleniumLogPath = (</> "selenium.log") <$> defaultPath

-- URLs. Might change at any moment, kinda why I'm putting them all together here.
-- | Url to download Selenium from. 
defaultSeleniumJarUrl :: String
defaultSeleniumJarUrl = "https://selenium-release.storage.googleapis.com/3.141/selenium-server-standalone-3.141.59.jar"

-- | API to get geckodriver's latest version.
geckoDriverVersionSource :: String
geckoDriverVersionSource = "https://api.github.com/repos/mozilla/geckodriver/releases/latest"

-- | Url to download geckodriver from. Always the latest version provided by `geckoDriverVersionSource`.
getGeckoDriverDownloadUrl :: String -> String
getGeckoDriverDownloadUrl version = [i|https://github.com/mozilla/geckodriver/releases/download/#{version}/geckodriver-#{version}-#{platform}#{format}|]
    where
        platform = desiredPlatform
        format = fileFormat

-- Platform-dependent variables. 
-- | Archive format for geckodriver's download. @.zip@ for Windows, @.tar.gz@ for everyone else. 
fileFormat :: String
fileFormat = case SI.os of
    "windows" -> ".zip"
    "mingw32" -> ".zip"

    "darwrin" -> ".tar.gz"
    "linux"   -> ".tar.gz"

    _         -> ".tar.gz"

-- | Platform this code is running at. The options are:
-- 
--      * win64
--      * win-aarch64
--      * win32
--      * macos
--      * macos-aarch64
--      * linux64
--      * linux-aarch64
--      * linux32
-- 
--  If the platform is not identified, @linux64@ is used. 
desiredPlatform :: String
desiredPlatform = case (SI.os, SI.arch) of
    ("windows", "x86_64")   -> "win64"
    ("windows", "aarch64")  -> "win-aarch64"
    ("windows", "i386")     -> "win32"
    ("mingw32", "x86_64")   -> "win64"
    ("mingw32", "aarch64")  -> "win-aarch64"
    ("mingw32", "i386")     -> "win32"

    ("darwin", "x86_64")    -> "macos"
    ("darwin", "aarch64")   -> "macos-aarch64"

    ("linux", "x86_64")     -> "linux64"
    ("linux", "aarch64")    -> "linux-aarch64"
    ("linux", "i386")       -> "linux32"

    _ -> "linux64"
