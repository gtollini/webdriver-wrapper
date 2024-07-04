{-# LANGUAGE OverloadedStrings #-}

module GeckoDriver (getGeckoDriver, getGeckoDriverVersion, getGeckoDriverIfNeeded) where

import qualified Data.Text as T
import Network.HTTP.Simple (setRequestMethod, httpLBS, parseRequest, setRequestHeader)
import Helpers (download, decompress)
import Constants (getGeckoDriverDownloadUrl, geckoDriverVersionSource, geckoArchivePath, geckoDriverPath, downloadPath)
import Network.HTTP.Client.Conduit (Response(responseBody))
import Data.Aeson (eitherDecode)
import Network.HTTP.Types (hUserAgent)
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson as A
import System.Directory (createDirectoryIfMissing, removeFile, doesFileExist)
import Control.Monad (unless)

getGeckoDriverIfNeeded :: IO ()
getGeckoDriverIfNeeded = do
    geckoPath <- geckoDriverPath
    hasGeckoDriver  <- doesFileExist geckoPath
    unless hasGeckoDriver getGeckoDriver

getGeckoDriver :: IO()
getGeckoDriver = do
    version <- getGeckoDriverVersion
    dPath <- downloadPath
    tarballPath <- geckoArchivePath

    createDirectoryIfMissing True dPath

    let url = getGeckoDriverDownloadUrl version
    download url tarballPath
    decompress tarballPath dPath
    removeFile tarballPath

getGeckoDriverVersion :: IO String
getGeckoDriverVersion = do
    requestUrl <- parseRequest versionAPI
    let
        request
            = setRequestMethod "GET"
            $ setRequestHeader hUserAgent ["cli"]
            requestUrl
    response <- responseBody <$> httpLBS request
    let
        version' = eitherDecode response  :: Either String A.Object
        maybeVersion = case version' of
            (Left err) -> error err
            (Right version'') -> AKM.lookup "tag_name" version''
        version = case maybeVersion of
            Nothing -> error "Couldn't parse response from GeckoDriver's version API"
            (Just (A.String v))-> v
            (Just _) -> error "\"tag_name\" key isn't returning a string. Maybe GeckoDriver's version API changed, consider opening a github issue."
    return $ T.unpack version

    where
        versionAPI = geckoDriverVersionSource
