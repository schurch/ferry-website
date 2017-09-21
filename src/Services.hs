{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Services
  ( Service
  , fetchServices
  , fetchService
  , serviceToJson
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson
import Database.Persist
import Database.Persist.MySQL
import Database.Persist.TH

connectionInfo :: ConnectInfo
connectionInfo =
  defaultConnectInfo
  { connectPort = 3306
  , connectUser = "root"
  , connectPassword = "password"
  , connectDatabase = "ferries"
  }

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Service sql=services json
        serviceId Int sql=service_id
        updated Int sql=updated
        sortOrder Int sql=sort_order
        area String sql=area
        route String sql=route
        status Int sql=status
        reason String Maybe sql=disruption_reason
        disruptionDate Int Maybe sql=disruption_date
        disruptionDetails String Maybe sql=disruption_details
        additionalInfo String Maybe sql=additional_info
        UniqueServiceId serviceId
        Primary serviceId
        deriving Show
    |]

fetchServices :: IO ([Entity Service])
fetchServices =
  runStderrLoggingT $
  withMySQLPool connectionInfo 10 $ \pool ->
    liftIO $ do
      flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        services <- selectList [] []
        return services

fetchService :: Int -> IO (Maybe (Entity Service))
fetchService serviceId =
  runStderrLoggingT $
  withMySQLPool connectionInfo 10 $ \pool ->
    liftIO $ do
      flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        service <- getBy $ UniqueServiceId serviceId
        return service

serviceToJson :: Entity Service -> Value
serviceToJson serviceEntity =
  let service = entityVal serviceEntity
  in object
       [ "serviceId" .= serviceServiceId service
       , "sortOrder" .= serviceSortOrder service
       , "area" .= serviceArea service
       , "route" .= serviceRoute service
       , "status" .= serviceStatus service
       , "updated" .= serviceUpdated service
       ]
