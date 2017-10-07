{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX
import Page
import Services
import Stylesheet
import System.Environment
import Web.Scotty

main :: IO ()
main = do
  args <- getArgs
  config <- decodeConfig $ head args
  scotty 3000 $ do
    get "/" $ do Web.Scotty.html $ page stylesheet
    staticPath "images"
    staticPath "fonts"
    get "/services" $ do
      services <- liftIO $ fetchServices config
      servicesCorrectStatus <- liftIO $ correctStatus services
      json $ serviceToCompactJson <$> servicesCorrectStatus
    get "/services/:id" $ do
      serviceId <- param "id"
      service <- liftIO $ fetchService config serviceId
      serviceCorrectStatus <- liftIO $ correctStatus service
      json $ serviceToJson <$> serviceCorrectStatus

staticPath :: String -> ScottyM ()
staticPath path =
  let routePattern = capture ("/" ++ path ++ "/:file")
  in get routePattern $ do
       fileName <- param "file"
       file $ "./" ++ path ++ "/" ++ fileName

correctStatus
  :: (Functor a)
  => a Service -> IO (a Service)
correctStatus a = do
  currentTime <- round <$> getPOSIXTime
  return $ (ensureStatusNotOutdated currentTime) <$> a
