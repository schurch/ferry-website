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
  , serviceToCompactJson
  , serviceToJson
  , ensureStatusNotOutdated
  ) where

import Config
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Aeson
import Data.Time.Clock.POSIX
import Data.Time.Format
import Database.Persist
import Database.Persist.MySQL
import Database.Persist.TH

data ServiceStatus
  = Normal
  | Disrupted
  | Cancelled
  | Unknown

instance Enum ServiceStatus where
  toEnum 0 = Normal
  toEnum 1 = Disrupted
  toEnum 2 = Cancelled
  toEnum (-99) = Unknown
  fromEnum Normal = 0
  fromEnum Disrupted = 2
  fromEnum Cancelled = 3
  fromEnum Unknown = (-99)

updatedMaxAge :: Int
updatedMaxAge = 60 * 30 -- 30 mins

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

fetchServices :: Config -> IO ([Service])
fetchServices config =
  runStderrLoggingT $
  withMySQLPool (configToConnectionInfo config) 10 $ \pool ->
    liftIO $ do
      flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        services <- selectList [] []
        time <- liftIO $ round <$> getPOSIXTime
        return $ (sanitizeService time) <$> services

fetchService :: Config -> Int -> IO (Maybe (Service))
fetchService config serviceId =
  runStderrLoggingT $
  withMySQLPool (configToConnectionInfo config) 10 $ \pool ->
    liftIO $ do
      flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        service <- getBy $ UniqueServiceId serviceId
        time <- liftIO $ round <$> getPOSIXTime
        return $ (sanitizeService time) <$> service

-- Json formatters
serviceToCompactJson :: Service -> Value
serviceToCompactJson service =
  object
    [ "service_id" .= serviceServiceId service
    , "sort_order" .= serviceSortOrder service
    , "area" .= serviceArea service
    , "route" .= serviceRoute service
    , "status" .= serviceStatus service
    , "updated" .= formatServiceTime (serviceUpdated service)
    ]

serviceToJson :: Service -> Value
serviceToJson service =
  object
    [ "service_id" .= serviceServiceId service
    , "sort_order" .= serviceSortOrder service
    , "area" .= serviceArea service
    , "route" .= serviceRoute service
    , "status" .= serviceStatus service
    , "updated" .= formatServiceTime (serviceUpdated service)
    , "disruption_reason" .= serviceReason service
    , "disruption_date" .=
      (formatServiceTime <$> (serviceDisruptionDate service))
    , "disruption_details" .= serviceDisruptionDetails service
    , "additional_info" .= serviceAdditionalInfo service
    ]

-- Helpers
sanitizeService :: Int -> Entity Service -> Service
sanitizeService currentTime =
  sanitizeDisruptionDate . (ensureStatusNotOutdated currentTime) . entityVal

ensureStatusNotOutdated :: Int -> Service -> Service
ensureStatusNotOutdated currentEpoch service =
  if currentEpoch - (serviceUpdated service) > updatedMaxAge
    then service {serviceStatus = fromEnum Unknown}
    else service

sanitizeDisruptionDate :: Service -> Service
sanitizeDisruptionDate service
  | (serviceDisruptionDate service) == Just 0 =
    service {serviceDisruptionDate = Nothing}
  | otherwise = service

formatServiceTime :: Int -> String
formatServiceTime epoch =
  formatTime
    Data.Time.Format.defaultTimeLocale
    "%Y-%m-%d %H:%M:%S UTC"
    (posixSecondsToUTCTime $ realToFrac epoch)

configToConnectionInfo :: Config -> ConnectInfo
configToConnectionInfo config =
  let databaseConfig' = databaseConfig config
  in defaultConnectInfo
     { connectPort = fromInteger (databasePort databaseConfig')
     , connectUser = (databaseUsername databaseConfig')
     , connectPassword = (databasePassword databaseConfig')
     , connectDatabase = (databaseName databaseConfig')
     }
