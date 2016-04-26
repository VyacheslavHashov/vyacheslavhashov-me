{-# LANGUAGE OverloadedStrings #-}

module Css(getCss) where

import Data.Monoid ((<>))
import Clay as C
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Clay.Media as CM

blueColor, yellowColor, whiteColor :: Color
blueColor   = "#41b6fc"
yellowColor = "#ffff88"
whiteColor  = white

emailClass', githubClass', googleClass', linkedinClass' :: Selector
emailClass'    = ".e"
githubClass'   = ".g"
googleClass'   = ".gg"
linkedinClass' = ".l"

getCss :: Text -> Text -> TL.Text
getCss bFont sFont = renderWith compact [] $ do
    baseFont bFont
    socialFont sFont
    baseCss

baseCss :: Css
baseCss = do
    star ? do
        sym padding (0 :: Size Abs)
        sym margin (0 :: Size Abs)
    body ? do
        backgroundColor blueColor
        color whiteColor
        fontFamily ["Raleway"] [sansSerif]
        "-webkit-font-smoothing" -: "antialiased"
        "-moz-osx-font-smoothing" -: "grayscale"
        textShadow (px 0) (px 1) (px 2) $ rgba 0 0 200 77
    i ? do
        margin (px 0) (px 15) (px 0) (px 15)
        "font-family" -: "'icomoon' !important"
        fontStyle normal
    emailClass' ? paddingRight (px 11)
    emailClass'    # before ? content (stringContent "\\f003")
    githubClass'   # before ? content (stringContent "\\f09b")
    googleClass'   # before ? content (stringContent "\\f0d5")
    linkedinClass' # before ? content (stringContent "\\f0e1")
    C.div ? do
        "margin" -: "auto"
        paddingTop (other "15vh")
    h1 ? do
        color yellowColor
        fontSize (px 36)
        paddingBottom (px 20)
    ".desc" ? do
        fontSize (px 30)
        paddingBottom (px 70)
    p <> h1 ? textAlign (other "center")
    ".social" ? fontSize (px 36)
    a ? do
        textDecoration (other "none")
        color whiteColor
        ":hover" & color yellowColor
    query CM.screen [CM.minHeight (px 400)] $ do
        C.div ? paddingTop (other "20vh")
        h1 ? paddingBottom (px 25)
        ".desc" ? paddingBottom (px 120)

socialFont :: Text -> Css
socialFont inlFont = fontFace $ do
    fontFamily ["icomoon"] []
    fontFaceSrc [FontFaceSrcUrl "i.eot" Nothing]
    fontFaceSrc [FontFaceSrcUrl inlFont (Just WOFF),
                 FontFaceSrcUrl "i.eot" (Just EmbeddedOpenType),
                 FontFaceSrcUrl "i.ttf" (Just TrueType),
                 FontFaceSrcUrl "i.svg" (Just SVG)]
    fontStyle normal
    fontWeight normal

baseFont :: Text -> Css
baseFont inlFont = fontFace $ do
    fontFamily ["Raleway"] []
    fontStyle normal
    fontWeight normal
    fontFaceSrc [FontFaceSrcUrl "r.eot" Nothing]
    fontFaceSrc [FontFaceSrcLocal "Raleway",
                 FontFaceSrcLocal "Raleway-Regular",
                 FontFaceSrcUrl inlFont (Just WOFF),
                 FontFaceSrcUrl "r.svg" (Just SVG),
                 FontFaceSrcUrl "r.eot" (Just EmbeddedOpenType),
                 FontFaceSrcUrl "r.ttf" (Just TrueType)]
