{-# LANGUAGE OverloadedStrings #-}

module Stylesheet
  ( stylesheet
  ) where

import Clay
import Clay.Flexbox
import qualified Clay.Media as Media
import Data.Text.Lazy

stylesheet :: Text
stylesheet =
  renderWith compact [] $ do
    mainCss
    query Media.screen [Media.maxWidth mobileBreakPoint] $ mobileCss

ferryBackgroundColor :: Color
ferryBackgroundColor = Other "#50C8C8"

mobileBreakPoint :: Size LengthUnit
mobileBreakPoint = (px 750)

mainCss :: Css
mainCss = do
  fontFace (fontFaceSrc [FontFaceSrcUrl "'/fonts/avenir.ttc'" Nothing])
  body ? do
    fontFamily ["avenir", "arial"] [sansSerif]
    sym margin (px 0)
  a ? color white
  h1 ? do
    fontWeight (weight 100)
    fontSize (pt 35)
    lineHeight (pct 100)
  h2 ? do
    fontWeight (weight 100)
    fontSize (pt 14)
  ".inner-container" ? do
    width mobileBreakPoint
    sym2 margin (px 0) auto
  "#header-container" ? do
    color white
    paddingTop (px 50)
    background
      (linearGradient
         (angular $ deg 180)
         [(ferryBackgroundColor, (pct 75)), (white, (pct 15))])
  "#header-container h2" ? do
    marginTop (px (-20))
    paddingBottom (px 5)
  "#header-container .inner-container" ? do
    display Clay.flex
    alignItems Clay.center
    flexDirection row
    justifyContent flexStart
  "#header-blurb" ? do
    marginTop (px (-80))
    marginBottom (px 40)
    sym2 padding (px 0) (px 0)
    textAlign start
  "#features-container" ? textAlign Clay.center
  "#feature-container" ? do
    display Clay.flex
    flexWrap Clay.Flexbox.wrap
    flexDirection row
    justifyContent flexStart
  ".single-feature-container" ? do
    width (pct 33.33333)
    sym2 margin (px 0) (px 0)
  ".single-feature-container h2" ? do
    fontWeight (weight 500)
    sym margin (px 0)
  ".single-feature-container p" ? do margin (px 0) (px 0) (px 30) (px 0)
  ".feature-icon" ? do
    float floatLeft
    paddingTop (px 4)
  ".feature-text" ? do
    float floatLeft
    textAlign start
    width (pct 75)
    marginLeft (px 15)
  "#support-container" ? do
    backgroundColor ferryBackgroundColor
    color white
    sym2 padding (px 20) (px 0)
    textAlign Clay.center
    margin (px 10) (px 0) (px 0) (px 0)
  "#support-container h1" ? sym margin (px 0)
  "#support-container p" ? sym2 padding (px 0) (px 20)

mobileCss :: Css
mobileCss = do
  ".inner-container" ? do
    width (pct 100)
    sym2 margin (px 0) (px 0)
  "#header-container .inner-container" ? do
    display Clay.flex
    alignItems Clay.center
    flexDirection column
    justifyContent Clay.center
  "#feature-container" ? do
    display Clay.flex
    flexWrap Clay.Flexbox.wrap
    flexDirection column
    justifyContent Clay.center
  "#feature-container" ? do
    display Clay.flex
    flexWrap Clay.Flexbox.wrap
    flexDirection column
    justifyContent Clay.center
  ".single-feature-container" ? do
    width (pct 80)
    sym2 margin (px 10) auto
  "#header-blurb" ? do
    marginTop (px 0)
    marginBottom (px 40)
    sym2 padding (px 0) (px 40)
    textAlign Clay.center
