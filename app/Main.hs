{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import System.Directory ( createDirectory
                        , copyFile
                        , doesDirectoryExist
                        , getDirectoryContents
                        , removeDirectoryRecursive
                        )
import System.FilePath.Posix ((</>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (encode)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)

import Html
import Css

staticDir, outputDir, fontDir, outputIndex :: FilePath
staticDir = "static"
outputDir = "output"
fontDir = "font"
outputIndex = outputDir </> "index.html"

main :: IO ()
main = do
    exists <- doesDirectoryExist outputDir
    if exists
        then removeDirectoryRecursive outputDir
        else pure ()
    createDirectory outputDir

    baseFont <- getInlinedFont $ fontDir </> "r.woff"
    socialFont <- getInlinedFont $ fontDir </> "i.woff"
    getStatic >>= mapM_ copyToOutput
    writeFile outputIndex . getHtml $ getCss baseFont socialFont
    where
        getStatic = filter f <$> getDirectoryContents staticDir
            where f name = name /= "." && name /= ".."
        copyToOutput name = copyFile (staticDir </> name ) (outputDir </> name)

encodeFont :: ByteString -> Text
encodeFont = ("data:application/font-woff;base64," <>) . decodeLatin1 . encode

getInlinedFont :: FilePath -> IO Text
getInlinedFont = (encodeFont <$>) . BS.readFile
