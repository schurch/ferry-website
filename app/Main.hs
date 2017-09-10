{-# LANGUAGE OverloadedStrings #-}

module Main where

import Page
import Stylesheet
import Web.Scotty

main :: IO ()
main =
  scotty 3000 $ do
    get "/" $ do Web.Scotty.html $ page stylesheet
    staticPath "images"
    staticPath "fonts"

staticPath :: String -> ScottyM ()
staticPath path =
  let routePattern = capture ("/" ++ path ++ "/:file")
  in get routePattern $ do
       fileName <- param "file"
       file $ "./" ++ path ++ "/" ++ fileName
