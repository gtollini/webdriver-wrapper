{-# LANGUAGE OverloadedStrings #-}

module Test.WebDriverWrapper.ChromeDriver (getChromeDriverIfNeeded) where

import Test.WebDriverWrapper.Constants (chromeDriverPath, downloadPath, chromeDriverVersionsUrl, chromeDriverArchIndex, chromeDriverArchivePath)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Control.Monad (unless, (<=<))
import Test.WebDriverWrapper.Helpers (download, decompressZip)
import Network.HTTP.Simple (parseRequest, setRequestMethod, httpLBS)
import Network.HTTP.Client.Conduit (Response(responseBody))
import Data.Aeson (eitherDecode)
import qualified Data.Aeson.KeyMap as AKM

getChromeDriverIfNeeded :: IO()
getChromeDriverIfNeeded = do
    chromeDriverPath' <- chromeDriverPath
    hasChromeDriver   <- doesFileExist chromeDriverPath'
    unless hasChromeDriver getChromeDriver

getChromeDriver :: IO()
getChromeDriver = do
    dPath   <- downloadPath
    chromeDriverArchivePath' <- chromeDriverArchivePath

    createDirectoryIfMissing True dPath

    url <- getChromeDriverDownloadUrl
    download url chromeDriverArchivePath'
    decompressZip chromeDriverArchivePath' dPath

getChromeDriverDownloadUrl :: IO String
getChromeDriverDownloadUrl = do
    requestUrl <- parseRequest chromeDriverVersionsUrl
    let
        request
            = setRequestMethod "GET"
            requestUrl
    response <- responseBody <$> httpLBS request

    let
        decodedResponse = eitherDecode response
        maybeVersions = case decodedResponse of
            (Left err)        -> error err
            (Right versions') -> AKM.lookup "versions" versions'
        maybeLastVersionUrls = (AKM.lookup "chromedriver" <=< AKM.lookup "downloads") . last =<< maybeVersions
        maybeLastVersionUrl  =  maybeLastVersionUrls >>= lookup chromeDriverArchIndex >>= AKM.lookup "url"
        url = case maybeLastVersionUrl of
            Nothing     -> error "Couldn't get chromedriver url!"
            (Just url') -> url'
    return url
