{-# LANGUAGE OverloadedStrings #-}

-- |Building up responses. This module provides a bunch of functions
-- to turn some primitive value into a handler, and the child modules
-- provide more complex handler composition.
module Web.Seacat.RequestHandler ( html200Response
                                 , html404Response
                                 , htmlResponse

                                 , utf8200Response
                                 , utf8404Response
                                 , utf8Response

                                 , bs200Response
                                 , bs404Response
                                 , bsResponse

                                 , respond
                                 , respondFile

                                 , redirect) where

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.Status
import Network.Wai (responseBuilder, responseFile, responseLBS)
import System.Directory (doesFileExist)
import System.FilePath.Posix (joinPath)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.Hamlet (HtmlUrl)
import Web.Routes.PathInfo (PathInfo)
import Web.Seacat.Configuration (conf')
import Web.Seacat.RequestHandler.Types

-- |Produce a 200 OK response from the given HTML. This calls
-- `htmlResponse`.
html200Response :: PathInfo r => HtmlUrl r -> Handler r
html200Response = htmlResponse ok200

-- |Produce a 404 File Not Found response form the given HTML. This
-- calls `htmlResponse`.
html404Response :: PathInfo r => HtmlUrl r -> Handler r
html404Response = htmlResponse notFound404

-- |Produce a response from the given HTML and response code. This
-- calls `respond`.
--
-- Todo: Do the second parameter of mkurl properly (get params)
htmlResponse :: PathInfo r => Status -> HtmlUrl r -> Handler r
htmlResponse status html = do mkurl <- askMkUrl
                              let builder = renderHtmlBuilder . html $ \a _ -> mkurl a []
                              respond status builder

-------------------------

-- |Produce a 200 OK response from the given UTF-8 text. This calls
-- `utf8Response`.
utf8200Response :: PathInfo r => Text -> Handler r
utf8200Response = utf8Response ok200

-- |Produce a 404 File Not Found response from the given UTF-8
-- text. This calls `utf8Response`.
utf8404Response :: PathInfo r => Text -> Handler r
utf8404Response = utf8Response notFound404

-- |Produce a response from the given UTF-8 text and response
-- code. This calls `respond`.
utf8Response :: PathInfo r => Status -> Text -> Handler r
utf8Response status = respond status . fromByteString . encodeUtf8

-------------------------

-- |Produce a 200 OK response from the given ByteString. This calls
-- `bsResponse`.
bs200Response :: PathInfo r => ByteString -> Handler r
bs200Response = bsResponse ok200

-- |Produce a 404 File Not Found response from the given
-- ByteString. This calls `bsResponse`.
bs404Response :: PathInfo r => ByteString -> Handler r
bs404Response = bsResponse notFound404

-- |Produce a response from the given ByteString and response
-- code. This calls `respond`.
bsResponse :: PathInfo r => Status -> ByteString -> Handler r
bsResponse status = respond status . fromByteString

-------------------------

-- |Produce a response from the given status and ByteString builder.
respond :: PathInfo r => Status -> Builder -> Handler r
respond status = return . responseBuilder status []

-- |Produce a response from the given file path (minus file
-- root). Call the provided 404 handler if the file isn't found.
respondFile :: PathInfo r => Handler r -> FilePath -> Handler r
respondFile on404 fp = do
  fileroot <- conf' "server" "file_root"
  let fullPath = joinPath [fileroot, fp]

  respondFile' on404 fullPath

-- |Produce a response from the given file path (including any
-- root). Call the provided 404 handler if the file isn't found.
--
-- This is somewhat unsafe as it lets you access files outside the
-- file root, hence why it isn't exported.
respondFile' :: PathInfo r => Handler r -> FilePath -> Handler r
respondFile' on404 fp = do
  exists <- liftIO $ doesFileExist fp
  if exists
  then return $ responseFile ok200 [] fp Nothing
  else on404

-- |Produce a response to redirect the user.
redirect :: PathInfo r => r -> Handler r
redirect url = do
  mkurl <- askMkUrl
  return $ responseLBS found302 [("Location", encodeUtf8 $ mkurl url [])] ""
