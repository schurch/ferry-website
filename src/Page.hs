{-# LANGUAGE OverloadedStrings #-}

module Page
  ( page
  ) where

import Control.Monad
import Data.Text.Lazy
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

data Feature = Feature
  { image :: String
  , featureTitle :: String
  , description :: String
  } deriving (Show)

page :: Text -> Text
page stylesheet = pack $ renderHtml $ htmlContent stylesheet

htmlContent :: Text -> Html
htmlContent stylesheet =
  docTypeHtml $ do
    H.head $ do
      H.title "Scottish Ferries App"
      H.meta ! name "viewport" !
        content "width=device-width, initial-scale=1.0, user-scalable=0"
      H.meta ! name "apple-itunes-app" ! content "app-id=861271891"
      H.style $ toHtml stylesheet
    body $ do
      H.div ! A.id "header-container" $ do
        H.div ! class_ "inner-container" $ do
          H.div ! A.id "header-blurb" $ do
            img ! src "images/icon.png" ! width "70px"
            h1 "Scottish Ferries"
            h2
              "Up to date disruption information for ferry services throughout Scotland."
            a !
              href
                "https://itunes.apple.com/gb/app/scottish-ferries/id861271891?mt=8" $ do
              img ! src "images/app-store.png" ! width "170px"
          H.div $ img ! src "images/screenshot.png" ! width "320px"
      H.div ! A.id "features-container" $ do
        H.div ! class_ "inner-container" $ do
          h1 "Features"
          H.div ! A.id "feature-container" $ do forM_ features featureToHtml
      H.div ! A.id "support-container" $ do
        H.div ! class_ "inner-container" $ do
          h1 "Support"
          p $ do
            "Please contact me at "
            linkText "stefan.church@gmail.com" "mailto:stefan.church@gmail.com"
            " if you have any issues or questions."

featureToHtml :: Feature -> Html
featureToHtml feature =
  H.div ! class_ "single-feature-container" $ do
    H.div ! class_ "feature-icon" $ do
      img ! src (stringValue (image feature)) ! width "30px"
    H.div ! class_ "feature-text" $ do
      h2 $ toHtml (featureTitle feature)
      p $ toHtml (description feature)

linkText :: String -> String -> Html
linkText text' link' = a ! href (stringValue link') $ toHtml text'

features :: [Feature]
features =
  [ Feature
      "images/wind.png"
      "Disruption info"
      "Live disruption infromtaion for ferry services"
  , Feature
      "images/notification.png"
      "Notifications"
      "Get notififed when the disrution status of a service changes"
  , Feature
      "images/clock.png"
      "Timetables"
      "Offline timetables and scheduled departures"
  , Feature
      "images/weather.png"
      "Weather"
      "Current weather forecast for each port"
  , Feature
      "images/phone.png"
      "Widget"
      "Widget allows quick access to details of last viewed service"
  , Feature "images/watch.png" "Watch App" "Keep updated on your wrist"
  ]
