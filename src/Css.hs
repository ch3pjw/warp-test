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

data ResponsiveCss = ResponsiveCss
  { rcGlobalCss :: Css
  , rcPhoneCss :: Css
  , rcLargeCss :: Css
  }

globalCss :: Css -> ResponsiveCss
globalCss css = ResponsiveCss css mempty mempty

phoneCss :: Css -> ResponsiveCss
phoneCss css = ResponsiveCss mempty css mempty

largeCss :: Css -> ResponsiveCss
largeCss css = ResponsiveCss mempty mempty css

instance Monoid ResponsiveCss where
  mempty = ResponsiveCss mempty mempty mempty
  (ResponsiveCss x y z) `mappend` (ResponsiveCss x' y' z') =
      ResponsiveCss (x <> x') (y <> y') (z <> z')


flattenResponsive :: Double -> ResponsiveCss -> Css
flattenResponsive boundary rc = do
    rcGlobalCss rc
    query screen [M.maxWidth (px $ boundary - 1)] $ rcPhoneCss rc
    query screen [M.minWidth $ px boundary] $ rcLargeCss rc


mainLayout :: ResponsiveCss
mainLayout =
  globalCss (do
    star ? do
      boxSizing borderBox

    html ? do
      height $ pct 100

    body ? do
      display flex
      flexFlow Fb.column Fb.nowrap
      justifyContent spaceBetween
      margin' nil
      -- Otherwise Firefox won't run the footer to the bottom:
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

    idRef "footer-wrapper" ? do
      Fb.flex 1 0 auto

    idRef "footer" ? do
      display flex
      flexFlow Fb.row Fb.wrap
      justifyContent spaceBetween
      paddingTop $ px 20
      paddingBottom $ px 20

    ul # byId "links" ? do
      padding' nil
      margin' nil

    idRef "links" ? li ? do
      display inline
      marginLeft $ px 10

    idRef "links" ? li # firstOfType ? do
      marginLeft nil

  ) <> phoneCss (do
      body ? do
        fontSize $ pct 120

      input ? do
        fontSize $ pct 90

      idRef "footer-wrapper" ? do
        marginTop $ px 30

      idRef "copyright" ? do
        order 2

      idRef "links" ? do
        order 1

  ) <> largeCss (do
      idRef "copyright" ? do
        order 1

      idRef "links" ? do
        order 2
  )

  where
    shadowGrey = grayish 204


mainStyling :: ResponsiveCss
mainStyling = globalCss (do
    importUrl $
         "https://fonts.googleapis.com/css?family"
         <> "=Source+Sans+Pro:300,400,500,700"
         <> "|Raleway:400,500"

    body ? do
      fontFamily ["Source Sans Pro"] [sansSerif]
      color offBlack
      fontSize $ px 18

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

    textInput <> emailInput <> submitButton ? do
      margin'' (px 5) nil
      padding' $ px 10
      minWidth $ px 200
      borderRadius' $ px 5
      fontSizeCustom smaller

    textInput <> emailInput ? do
      border solid (px 1) darkgray;
      backgroundColor offWhite

    submitButton ? do
      cursor pointer
      background $ vGradient goGreen' goGreen
      border' nil;
      color white
      fontWeight $ weight 700

    submitButton # hover ? do
      background $ vGradient goGreenH goGreenH'

    submitButton # active ? do
      paddingTop $ px 11  -- Whatever it was +1
      paddingBottom $ px 9 -- Whatever it was -1
      "background" -: "none"
      backgroundColor goGreenH
      -- insetBoxShadow solid nil nil (px 5) "#777777"
      "box-shadow" -: "0 0 5px #777777 inset"

    input # ".error" ? do
      border solid (px 1) recordRed
      backgroundColor $ hsl 5 85 98

    label # ".error" ? do
      color recordRed
      fontSizeCustom smaller

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
  ) <> phoneCss (do
    ".large-screen" ? do
      display none
  ) <> largeCss (do
    ".small-screen" ? do
      display none
  )
  where
    offBlack = hsl 300 22 10
    offWhite = hsl 0 0 98
    goGreen = hsl 121 78 37
    goGreen' = hsl 138 78 44
    goGreenH = hsl 138 78 42
    goGreenH' = hsl 121 78 35
    recordRed = hsl 5 82 51
    ip t = input # ("type" @= t)
    textInput = ip "text"
    emailInput = ip "email"
    submitButton = ip "submit"


emailSubmissionCss :: ResponsiveCss
emailSubmissionCss = globalCss (do
    idRef "content" ? do
      display grid

    idRef "registration-form" ? do
      alignSelf center

    form ? do
      display flex
      flexFlow Fb.column Fb.nowrap
  ) <> phoneCss (do
      idRef "content" ? do
        "grid-template-columns" -: "auto"
        "grid-row-gap" -: "20 px"
  ) <> largeCss (do
      idRef "content" ? do
        "grid-template-columns" -: "60% 40%"
        "grid-column-gap" -: "50px"
        padding' $ px 75
  )


notificationCss :: ResponsiveCss
notificationCss = globalCss (do
    h1 ? do
      fontSize $ em 2.5
  ) <> largeCss (do
    idRef "content" ? do
      padding' $ px 75
    idRef "content" ? p ? do
      width $ px 450
  )


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
