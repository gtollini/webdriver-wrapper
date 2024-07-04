{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Constants (geckoDriverPath, defaultPath, defaultSeleniumJarUrl, desiredPlatform, getGeckoDriverDownloadUrl, geckoDriverVersionSource, downloadPath, geckoArchivePath, fileFormat, seleniumPath, seleniumLogPath) where

import Data.String.Interpolate (i)
import qualified System.Info as SI
import System.Directory (getXdgDirectory, XdgDirectory (XdgData))
import System.FilePath ((</>))

-- Paths. Should be platform independent.
defaultPath :: IO FilePath
defaultPath = getXdgDirectory XdgData "haskell-webdriver-wrapper"

downloadPath :: IO FilePath
downloadPath = (</> desiredPlatform) <$> defaultPath

geckoArchivePath :: IO FilePath
geckoArchivePath = (</> archive) <$> downloadPath
    where
        archive = "geckodriver" <> fileFormat

geckoDriverPath :: IO FilePath
geckoDriverPath = (</> "geckodriver") <$> downloadPath

seleniumPath :: IO FilePath
seleniumPath = (</> "selenium.jar") <$> downloadPath

seleniumLogPath :: IO FilePath
seleniumLogPath = (</> "selenium.log") <$> defaultPath

-- URLs. Might change at any moment, kinda why I'm putting them all together here.
defaultSeleniumJarUrl :: String
defaultSeleniumJarUrl = "https://selenium-release.storage.googleapis.com/3.141/selenium-server-standalone-3.141.59.jar"

geckoDriverVersionSource :: String
geckoDriverVersionSource = "https://api.github.com/repos/mozilla/geckodriver/releases/latest"

getGeckoDriverDownloadUrl :: String -> String
getGeckoDriverDownloadUrl version = [i|https://github.com/mozilla/geckodriver/releases/download/#{version}/geckodriver-#{version}-#{platform}#{format}|]
    where
        platform = desiredPlatform
        format = fileFormat

-- Platform-dependent variables. 
fileFormat :: String
fileFormat = case SI.os of
    "windows" -> ".zip"
    "mingw32" -> ".zip"

    "darwrin" -> ".tar.gz"
    "linux"   -> ".tar.gz"

    _         -> ".tar.gz"

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
