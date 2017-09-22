{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Applicative
import Data.Maybe (fromJust)
import Data.Yaml (FromJSON(..), (.:))
import qualified Data.Yaml as Y
import Prelude

decodeConfig :: String -> IO (Config)
decodeConfig filePath = do
  fmap fromJust $ Y.decodeFile filePath

data Config = Config
  { databaseConfig :: DatabaseConfig
  , parseConfig :: ParseConfig
  , marineTrafficConfig :: MarineTrafficConfig
  } deriving (Eq, Show)

data DatabaseConfig = DatabaseConfig
  { databaseHost :: String
  , databaseName :: String
  , databasePort :: Integer
  , databaseUsername :: String
  , databasePassword :: String
  } deriving (Eq, Show)

data ParseConfig = ParseConfig
  { parseEndpoint :: String
  , parseApplicationId :: String
  } deriving (Eq, Show)

data MarineTrafficConfig = MarineTrafficConfig
  { marineTrafficApiKey :: String
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$> v .: "database" <*> v .: "parse" <*> v .: "marinetraffic"
  parseJSON _ = fail "Expected Object for Config value"

instance FromJSON DatabaseConfig where
  parseJSON (Y.Object v) =
    DatabaseConfig <$> v .: "host" <*> v .: "database" <*> v .: "port" <*>
    v .: "username" <*>
    v .: "password"
  parseJSON _ = fail "Expected Object for DatabaseConfig value"

instance FromJSON ParseConfig where
  parseJSON (Y.Object v) =
    ParseConfig <$> v .: "endpoint" <*> v .: "application_id"
  parseJSON _ = fail "Expected Object for ParseConfig value"

instance FromJSON MarineTrafficConfig where
  parseJSON (Y.Object v) = MarineTrafficConfig <$> v .: "api_key"
  parseJSON _ = fail "Expected Object for MarineTrafficConfig value"
