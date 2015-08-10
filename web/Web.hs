{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
import           Control.Monad
import           Control.Monad.Logger         (runStderrLoggingT)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Aeson                   (decodeStrict, encode)
import qualified Data.ByteString.Lazy.Char8   as BSL
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Database.Persist.Sqlite
import           Yesod

import           Run

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
RunEntry
    uuid String
    json String
    deriving Show
|]

data App = App ConnectionPool

mkYesod "App" [parseRoutes|
/getruns GetRunsR GET
/getrun/#Text GetRunR GET
/postrun PostRunR POST
|]

instance Yesod App

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    App pool <- getYesod
    runSqlPool action pool

getGetRunsR :: Handler Value
getGetRunsR = do
  runEntities <- runDB $ selectList [] []
  let uuids = map (runEntryUuid . entityVal) runEntities
  addHeader "Access-Control-Allow-Origin" "*"
  return $ toJSON uuids

getGetRunR :: Text -> Handler TypedContent
getGetRunR desiredUuid = do
  maybeRunEntity <- runDB $ selectFirst [ RunEntryUuid ==. (T.unpack desiredUuid) ] []
  addHeader "Access-Control-Allow-Origin" "*"
  case maybeRunEntity of
    Nothing -> error "no such run"
    Just runEntity -> respond typeJson (runEntryJson $ entityVal runEntity)

postPostRunR :: Handler String
postPostRunR = do
  maybeJsonString <- lookupPostParam "json"
  let run = fromMaybe (error "couldn't parse run") $ do
        jsonString <- maybeJsonString
        decodeStrict $ T.encodeUtf8 jsonString

  let sentUuid = runUUID run

  maybeExistingRun <-
    runDB $ selectFirst [ RunEntryUuid ==. sentUuid ] []

  case maybeExistingRun of
    Nothing -> do
      void $ runDB $ insert $ RunEntry sentUuid (BSL.unpack (encode run))
      return $ "thanks for run " ++ sentUuid
    Just _ ->
      return $ "already got " ++ sentUuid
    
main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3" 10 $ \pool -> liftIO $ do
  runResourceT $ flip runSqlPool pool $ do
    runMigration migrateAll
  warp 3000 $ App pool
