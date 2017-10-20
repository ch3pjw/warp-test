{-# LANGUAGE OverloadedStrings #-}

module Css where

import Data.Monoid

import Clay
import qualified Clay as C
import qualified Clay.Flexbox as Fb
import Clay.Media (screen)
import qualified Clay.Media as M

z :: Size LengthUnit
z = (px 0)

mainLayout :: Css
mainLayout = do
    body ? do
      display flex
      flexFlow Fb.column Fb.nowrap
      margin' z
      height $ pct 100

    body |> star |> C.div ? do
      maxWidth $ px 820
      margin' auto
      padding'' z (px 10)

    "#header-wrapper" ? do
      boxShadow z (px 5) (px 15) shadowGrey

    "#header" ? do
      display flex
      justifyContent spaceBetween
      alignItems center
      vMargin $ px 10

    "#content" ? do
      display grid

    query screen [M.maxWidth $ deviceSizeBoundary - (px 1)] $
      "#content" ? do
        "grid-template-columns" -: "auto"
        "grid-column-gap" -: "20 px"

    query screen [M.minWidth deviceSizeBoundary] $
      "content" ? do
        "grid-template-columns" -: "60% 40%"
        "grid-column-gap" -: "50 px"
        padding' $ px 75

    "#registration-form" ? do
      alignSelf center

    form ? do
      display flex
      flexFlow Fb.column Fb.nowrap

    "#footer-wrapper" ? do
      Fb.flex 1 0 auto

    "#footer" ? do
      display flex
      justifyContent spaceBetween
      paddingTop $ px 20
      paddingBottom $ px 20

    "#links" |> star ? do
      marginLeft $ px 10


  where
    shadowGrey = grayish 204
    deviceSizeBoundary = (px 600)


mainStyling :: Css
mainStyling = do
    importUrl $
         "https://fonts.googleapis.com/css?family"
         <> "=Source+Sans+Pro:300,400,500,700"
         <> "|Raleway:400,500"

    body ? do
      fontFamily ["Source Sans Pro"] [sansSerif]
      color offBlack

    (h1 <> h2 <> h3) ? do
      fontFamily ["Raleway"] [sansSerif]
      fontWeight $ weight 500

    aside ? do
      fontSizeCustom smaller
      color $ grayish 153

    a # href # hover ? do
      color recordRed

    "#header" ? a ? do
      textDecoration none
      color inherit
      fontWeight $ weight 500

    "#header" ? a # hover ? do
      color recordRed

    inputField <> submitButton ? do
      margin'' (px 5) z
      padding' $ px 10
      minWidth $ px 200
      borderRadius' $ px 5

    inputField ? do
      border solid (px 1) darkgray;
      backgroundColor offWhite

    submitButton ? do
      cursor pointer
      background $ vGradient goGreen' goGreen

    submitButton # hover ? do
      background $ vGradient goGreenH' goGreenH

    submitButton # active ? do
      paddingTop $ px 11  -- Whatever it was +1
      paddingBottom $ px 9 -- Whatever it was -1
      -- background none
      backgroundColor goGreenH
      insetBoxShadow solid z z (px 5) "#777777"

    "#footer-wrapper" ? do
      backgroundColor $ grayish 238
      boxShadow z z (px 15) $ grayish 204

    "#footer" ? do
      color $ grayish 119
      fontWeight $ weight 300

    "#footer" ? a ? do
      textDecoration none
      color inherit

    "#footer" ? a # hover ? do
      color recordRed
  where
    offBlack = hsl 300 22 10
    offWhite = hsl 0 0 98
    goGreen = hsl 121 78 37
    goGreen' = hsl 138 78 44
    goGreenH = hsl 138 78 42
    goGreenH' = hsl 121 78 35
    recordRed = hsl 5 82 51
    inputField = input # ("type" @= "text")
    submitButton = input # ("type" @= "submit")


padding' :: Size a -> Css
padding' x = padding x x x x

padding'' :: Size a -> Size a -> Css
padding'' y x = padding y x y x

margin' :: Size a -> Css
margin' x = margin x x x x

margin'' :: Size a -> Size a -> Css
margin'' y x = margin y x y x

vMargin :: Size a -> Css
vMargin x = marginTop x >> marginBottom x

borderRadius' :: Size a -> Css
borderRadius' x = borderRadius x x x x

borderRadius'' :: Size a -> Size a -> Css
borderRadius'' y x = borderRadius y x y x
