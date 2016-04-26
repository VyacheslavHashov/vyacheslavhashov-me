{-# LANGUAGE OverloadedStrings #-}

module Html (getHtml) where

import Text.Blaze.Html.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text.Lazy as T


emailLink, githubLink, googleLink, linkedinLink :: String
emailLink    = "mailto:vyacheslavhashov@gmail.com"
githubLink   = "#"
googleLink   = "#"
linkedinLink = "#"

emailClass, githubClass, googleClass, linkedinClass :: H.AttributeValue
emailClass    = "e"
githubClass   = "g"
googleClass   = "gg"
linkedinClass = "l"


getHtml :: T.Text -> String
getHtml = renderHtml . html

html :: T.Text -> H.Html
html style = H.docTypeHtml H.! A.lang "en" $ do
    H.head $ do
        H.title "Vyacheslav Hashov"
        H.meta H.! A.httpEquiv "Content-Type" H.! A.content "text/html; charset=utf-8"
        H.meta H.! A.httpEquiv "X-UA-Compatible" H.! A.content "IE=edge"
        H.link H.! A.rel "icon" H.! A.href "favicon.ico"
        H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1.0"
        H.meta H.! A.name "description" H.! A.content "Vyacheslav Hashov's personal page"
        H.meta H.! A.name "keywords" H.! A.content "Vyacheslav Hashov, Вячеслав Хашов"
        H.style H.! A.type_ "text/css" $ H.toHtml style
    H.body $ H.div $ do
        H.h1 "Vyacheslav Hashov"
        H.p H.! A.class_ "desc" $ "One sentence pitch here"
        H.p H.! A.class_ "social" $ do
            H.a H.! A.href (H.toValue $ encodeEmail emailLink) $ H.i H.! A.class_ emailClass $ ""
            H.a H.! A.href (H.toValue githubLink) $   H.i H.! A.class_ githubClass $ ""
            H.a H.! A.href (H.toValue googleLink) $   H.i H.! A.class_ googleClass $ ""
            H.a H.! A.href (H.toValue linkedinLink) $ H.i H.! A.class_ linkedinClass $ ""
        H.script H.! A.type_ "text/javascript" $ H.toHtml javascript

encodeEmail :: String -> String
encodeEmail = go 0
    where
        key = 19
        go :: Int -> String -> String
        go _ [] = ""
        go prev (x:xs) = let c = toEnum . (\ch -> ch + prev - key) $ fromEnum x
                         in c : go (fromEnum x) xs


-- | compiled by Google Closure Compiler
-- Source:
--
-- node = document.getElementsByClassName('e')[0].parentNode;
-- link = node.getAttribute('href');
-- var newLink = Array();
-- var prev = 0;
-- var key = 19;
-- for (var i = 0; i < link.length; i++) {
--     newLink[i] = link[i].charCodeAt() + key - prev;
--     prev = newLink[i];
-- }
-- node.setAttribute('href', String.fromCharCode.apply(String, newLink));
javascript :: String
javascript = "n=document.getElementsByClassName('e')[0].parentNode;\
             \l=n.getAttribute('href');for(var a=[],b=0,c=0;c<l.length;c++)\
             \a[c]=l[c].charCodeAt()+19-b,\
             \b=a[c];n.setAttribute('href',String.fromCharCode.apply(String,a));"
