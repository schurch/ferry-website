{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Page
import Services
import Stylesheet
import Web.Scotty

main :: IO ()
main =
  scotty 3000 $ do
    get "/" $ do Web.Scotty.html $ page stylesheet
    staticPath "images"
    staticPath "fonts"
    get "/services" $ do
      services <- liftIO $ fetchServices
      json $ fmap serviceToJson services
    get "/services/:id" $ do
      serviceId <- param "id"
      service <- liftIO $ fetchService serviceId
      json service

staticPath :: String -> ScottyM ()
staticPath path =
  let routePattern = capture ("/" ++ path ++ "/:file")
  in get routePattern $ do
       fileName <- param "file"
       file $ "./" ++ path ++ "/" ++ fileName
