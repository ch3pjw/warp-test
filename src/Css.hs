{-# LANGUAGE OverloadedStrings #-}

module Css where

import Data.Monoid
import Data.Text (Text)

import Clay
import qualified Clay as C
import qualified Clay.Flexbox as Fb
import Clay.Media (screen)
import qualified Clay.Media as M
import Clay.Stylesheet (key)
import Clay.Selector (
  SelectorF(..), Refinement(..), Predicate(Id), Path(Star), Fix(In))

mainLayout :: Css
mainLayout = do
    body ? do
      display flex
      flexFlow Fb.column Fb.nowrap
      margin' nil
      height $ pct 100

    body |> star |> C.div ? do
      maxWidth $ px 820
      margin' auto
      padding'' nil (px 10)

    idRef "header-wrapper" ? do
      boxShadow nil (px 5) (px 15) shadowGrey

    idRef "header" ? do
      display flex
      justifyContent spaceBetween
      alignItems center
      vMargin $ px 10

    idRef "content" ? do
      display grid

    query screen [M.maxWidth $ deviceSizeBoundary - (px 1)] $
      idRef "content" ? do
        "grid-template-columns" -: "auto"
        "grid-column-gap" -: "20 px"

    query screen [M.minWidth deviceSizeBoundary] $
      idRef "content" ? do
        "grid-template-columns" -: "60% 40%"
        "grid-column-gap" -: "50 px"
        padding' $ px 75

    idRef "registration-form" ? do
      alignSelf center

    form ? do
      display flex
      flexFlow Fb.column Fb.nowrap

    idRef "footer-wrapper" ? do
      Fb.flex 1 0 auto

    idRef "footer" ? do
      display flex
      flexFlow Fb.row Fb.wrap
      justifyContent spaceBetween
      paddingTop $ px 20
      paddingBottom $ px 20

    idRef "links" |> star ? do
      marginLeft $ px 10

    idRef "links" |> star # firstOfType ? do
      marginLeft nil


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

    idRef "header" ? a ? do
      textDecoration none
      color inherit
      fontWeight $ weight 500

    idRef "header" ? a # hover ? do
      color recordRed

    inputField <> submitButton ? do
      margin'' (px 5) nil
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
      insetBoxShadow solid nil nil (px 5) "#777777"

    idRef "footer-wrapper" ? do
      backgroundColor $ grayish 238
      boxShadow nil nil (px 15) $ grayish 204

    idRef "footer" ? do
      color $ grayish 119
      fontWeight $ weight 300

    idRef "footer" ? a ? do
      textDecoration none
      color inherit

    idRef "footer" ? a # hover ? do
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
padding' x = key "padding" x

padding'' :: Size a -> Size a -> Css
padding'' y x = key "padding" (y ! x)

margin' :: Size a -> Css
margin' x = key "margin" x

margin'' :: Size a -> Size a -> Css
margin'' y x = key "margin" (y ! x)

vMargin :: Size a -> Css
vMargin y = marginTop y >> marginBottom y

hMargin :: Size a -> Css
hMargin x = marginLeft x >> marginRight x

borderRadius' :: Size a -> Css
borderRadius' x = borderRadius x x x x

borderRadius'' :: Size a -> Size a -> Css
borderRadius'' y x = borderRadius y x y x

border' :: Size a -> Css
border' x = key "border" x


-- | The IsString-inferred way of specifying ID selectors is error prone (one
--   can type "foo" instead of "#foo" all too easily. The `byId` function from
--   Clay only prevides a Refinement, not a Selector. So, we implement our own
--   ID Selector by borrowing from Clay's internals.
idRef :: Text -> Selector
idRef identifier = In $ SelectorF (Refinement [Id identifier]) Star
