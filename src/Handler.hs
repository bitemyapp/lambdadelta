{-# LANGUAGE OverloadedStrings #-}

module Handler where

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Configuration (conf')
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.Status
import Network.Wai (responseBuilder, responseFile, responseLBS)
import Routes
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.Hamlet (HtmlUrl)
import System.Directory (doesFileExist)
import System.FilePath.Posix (joinPath)
import Types

import qualified Handler.Templates as T (error)

-- |Produce a 200 OK response from the given HTML
html200Response :: HtmlUrl Sitemap -> Handler
html200Response html = htmlResponse ok200 html

-- |Produce a 404 File Not Found response form the given HTML
html404Response :: HtmlUrl Sitemap -> Handler
html404Response html = htmlResponse notFound404 html

-- |Produce a response from the given HTML and response code
-- Todo: Do the second parameter of mkurl properly (get params)
htmlResponse :: Status -> HtmlUrl Sitemap -> Handler
htmlResponse status html = do mkurl <- askMkUrl
                              let builder = renderHtmlBuilder . html $ \a _ -> mkurl a []
                              respond status builder

-------------------------

-- |Produce a 200 OK response from the given UTF-8 text
utf8200Response :: Text -> Handler
utf8200Response = utf8Response ok200

-- |Produce a 404 File Not Found response from the given UTF-8 text
utf8404Response :: Text -> Handler
utf8404Response = utf8Response notFound404

-- |Produce a response from the given UTF-8 text and response code
utf8Response :: Status -> Text -> Handler
utf8Response status = respond status . fromByteString . encodeUtf8

-------------------------

-- |Produce a 200 OK response from the given ByteString
bs200Response :: ByteString -> Handler
bs200Response = bsResponse ok200

-- |Produce a 404 File Not Found response from the given ByteString
bs404Response :: ByteString -> Handler
bs404Response = bsResponse notFound404

-- |Produce a response from the given ByteString and response code
bsResponse :: Status -> ByteString -> Handler
bsResponse status = respond status . fromByteString

-------------------------

-- |Produce a response from the given status and ByteString builder
respond :: Status -> Builder -> Handler
respond status = return . responseBuilder status []

-- |Produce a response from the given file path (minus file root). If
-- the file doesn't exist, a 404 error is raised.
respondFile :: FilePath -> Handler
respondFile fp = do fileroot <- conf' "server" "file_root"
                    let fullPath = joinPath [fileroot, fp]

                    respondFile' fullPath

-- |Produce a response from the given file path (including any
-- root). If the file doesn't exist, a 404 error is raised.
respondFile' :: FilePath -> Handler
respondFile' fp = do exists <- liftIO $ doesFileExist fp
                     if exists
                     then return $ responseFile ok200 [] fp Nothing
                     else htmlResponse notFound404 $ T.error notFound404 "File not found"

-- |Produce a response to redirect the user
redirect :: Sitemap -> Handler
redirect url = do
  mkurl <- askMkUrl
  return $ responseLBS found302 [("Location", encodeUtf8 $ mkurl url [])] ""
