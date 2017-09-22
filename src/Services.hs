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

import Config
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson
import Database.Persist
import Database.Persist.MySQL
import Database.Persist.TH

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

fetchServices :: Config -> IO ([Entity Service])
fetchServices config =
  runStderrLoggingT $
  withMySQLPool (configToConnectionInfo config) 10 $ \pool ->
    liftIO $ do
      flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        services <- selectList [] []
        return services

fetchService :: Config -> Int -> IO (Maybe (Entity Service))
fetchService config serviceId =
  runStderrLoggingT $
  withMySQLPool (configToConnectionInfo config) 10 $ \pool ->
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
       , "sort_order" .= serviceSortOrder service
       , "area" .= serviceArea service
       , "route" .= serviceRoute service
       , "status" .= serviceStatus service
       , "updated" .= serviceUpdated service
       ]

configToConnectionInfo :: Config -> ConnectInfo
configToConnectionInfo config =
  let databaseConfig' = databaseConfig config
  in defaultConnectInfo
     { connectPort = fromInteger (databasePort databaseConfig')
     , connectUser = (databaseUsername databaseConfig')
     , connectPassword = (databasePassword databaseConfig')
     , connectDatabase = (databaseName databaseConfig')
     }
