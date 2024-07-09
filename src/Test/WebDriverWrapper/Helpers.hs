{-# LANGUAGE OverloadedStrings #-}
{- |
Module : Test.WebDriverWrapper.Helpers
Description : Generic functions.
-}
module Test.WebDriverWrapper.Helpers (download, decompress, decompressZip) where

import Test.WebDriverWrapper.Constants (fileFormat, geckoDriverPath)
import Network.HTTP.Simple (setRequestHeader, setRequestMethod, httpLBS)
import Network.HTTP.Types (hUserAgent)
import Network.HTTP.Conduit (Response(..), parseRequest)
import qualified Data.ByteString.Lazy as BS
import Codec.Archive.Zip (toArchive, fromArchive)
import qualified Codec.Compression.GZip as G
import qualified Codec.Archive.Tar as Tar
import System.Posix ( setFileMode, accessModes )

-- | Downloads from @url@ at @output@ filepath. 
download :: String -> FilePath -> IO()
download url output = do
    requestUrl <- parseRequest url
    let
        request
            = setRequestMethod "GET"
            $ setRequestHeader hUserAgent ["cli"]
            requestUrl
    response <- responseBody <$> httpLBS request
    BS.writeFile output response

-- | Decompresses geckodriver's download, which comes in @.zip@ for Windows or @.tar.gz@ for everyone else. 
-- Takes in the archive's filepath and the output filepath. 
decompress :: FilePath -> FilePath -> IO()
decompress file outputPath = do
    case fileFormat of
        ".zip"    -> decompressZip file outputPath
        ".tar.gz" -> decompressTarball file outputPath
        _ -> error "unknown file"

decompressZip :: FilePath -> FilePath -> IO()
decompressZip file outputPath = do
    archive <- toArchive <$> BS.readFile file
    BS.writeFile outputPath $ fromArchive archive

decompressTarball :: FilePath -> FilePath -> IO()
decompressTarball file outputPath = do
    tarball <- G.decompress <$> BS.readFile file
    Tar.unpack outputPath $ Tar.read tarball

    geckoDriver <- geckoDriverPath

    setFileMode geckoDriver accessModes