{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe
import           Network.HTTP.Client

import           Run

main = do
  jsonStrings <- BS.lines <$> BS.getContents
  let runs = map (fromJust . decodeStrict') jsonStrings
  withManager defaultManagerSettings $ \manager -> do
    forM_ runs $ \r -> do
      request <-
        urlEncodedBody [("json", BSL.toStrict (encode (r :: Run)))] <$>
        parseUrl "http://localhost:3000/postrun"
      response <- httpLbs request manager
      print $ responseBody response
  return ()
