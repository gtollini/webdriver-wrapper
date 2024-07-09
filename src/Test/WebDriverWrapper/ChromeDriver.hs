{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Test.WebDriverWrapper.ChromeDriver (getChromeDriverIfNeeded, getChromeDriverDownloadUrl) where

import Test.WebDriverWrapper.Constants (chromeDriverPath, downloadPath, chromeDriverVersionsUrl, chromeDriverArchIndex, chromeDriverArchivePath)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Control.Monad (unless)
import Test.WebDriverWrapper.Helpers (download, decompressZip)
import Network.HTTP.Simple (parseRequest, setRequestMethod, httpLBS)
import Network.HTTP.Client.Conduit (Response(responseBody))
import Data.Aeson (eitherDecode)
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.Text as T
import GHC.Generics (Generic)

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
    download "url" chromeDriverArchivePath'
    -- download url chromeDriverArchivePath'
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
        decodedResponse = eitherDecode response :: Either String ChromeDriverMain

        versionDownloads = case decodedResponse of
            (Left err)        -> error err
            (Right versions') -> downloads $ V.last $ versions  versions'
        maybeLastVersionUrl  =  do
            chromedriver <- AKM.lookup "chromedriver" versionDownloads 
            platform <- chromedriver V.!? chromeDriverArchIndex
            AKM.lookup "url" platform
        url = case maybeLastVersionUrl of
            Nothing     -> error "Couldn't get chromedriver url!"
            (Just url') -> T.unpack url'
    return url

-- To help out Aeson in parsing the JSON.
data ChromeDriverMain = ChromeDriverMain{
    timestamp :: T.Text,
    versions :: V.Vector ChromeDriverVersion
}
  deriving (Show, Generic, A.ToJSON, A.FromJSON)

data ChromeDriverVersion = ChromeDriverVersion{
    version :: T.Text,
    revision :: T.Text,
    downloads :: AKM.KeyMap (V.Vector (AKM.KeyMap T.Text))
}
  deriving (Show, Generic, A.ToJSON, A.FromJSON)
