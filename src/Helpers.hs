{-# LANGUAGE OverloadedStrings #-}
{- |
Module : Helpers
Description : Generic functions.
-}
module Helpers (download, decompress) where

import Constants (fileFormat)
import Network.HTTP.Simple (setRequestHeader, setRequestMethod, httpLBS)
import Network.HTTP.Types (hUserAgent)
import Network.HTTP.Conduit (Response(..), parseRequest)
import qualified Data.ByteString.Lazy as BS
import Codec.Archive.Zip (toArchive, fromArchive)
import qualified Codec.Compression.GZip as G
import qualified Codec.Archive.Tar as Tar

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
        ".zip"    -> do
            archive <- toArchive <$> BS.readFile file
            BS.writeFile outputPath $ fromArchive archive
        ".tar.gz" -> do   
            tarball <- G.decompress <$> BS.readFile file
            Tar.unpack outputPath $ Tar.read tarball
        _ -> error "unknown file"
