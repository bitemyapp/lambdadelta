{-# LANGUAGE OverloadedStrings #-}

-- |Building up responses. This module provides a bunch of functions
-- to turn some primitive value into a handler, and the child modules
-- provide more complex handler composition.
module Web.Seacat.RequestHandler ( htmlResponse
                                 , htmlResponse'

                                 , htmlUrlResponse
                                 , htmlUrlResponse'

                                 , textResponse
                                 , textResponse'

                                 , textUrlResponse
                                 , textUrlResponse'

                                 , respond
                                 , respondFile

                                 , redirect) where

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.Status (Status, ok200, found302)
import Network.Wai (responseBuilder, responseFile, responseLBS)
import System.Directory (doesFileExist)
import System.FilePath.Posix (joinPath)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Web.Routes.PathInfo (PathInfo)
import Web.Seacat.Configuration (conf')
import Web.Seacat.RequestHandler.Types

-- |Produce a 200 OK response from the given HTML. This calls
-- `htmlResponse'`.
htmlResponse :: PathInfo r => Html -> Handler r
htmlResponse = htmlResponse' ok200

-- |Produce a response from the given HTML and response code. This
-- calls `respond`.
htmlResponse' :: PathInfo r => Status -> Html -> Handler r
htmlResponse' status html = respond status $ renderHtmlBuilder html

-------------------------

-- |Produce a 200 OK response from the given HTML-generating
-- function. This calls `htmlUrlResponse'`.
htmlUrlResponse :: PathInfo r => (MkUrl r -> Html) -> Handler r
htmlUrlResponse = htmlUrlResponse' ok200

-- |Produce a response from the given HTML-generating function and
-- response code. This calls `respond`.
htmlUrlResponse' :: PathInfo r => Status -> (MkUrl r -> Html) -> Handler r
htmlUrlResponse' status html = do
  mkurl <- askMkUrl
  let builder = renderHtmlBuilder $ html mkurl
  respond status builder

-------------------------

-- |Produce a 200 OK response from the given UTF-8 text. This calls
-- `textResponse'`.
textResponse :: PathInfo r => Text -> Handler r
textResponse = textResponse' ok200

-- |Produce a response from the given UTF-8 text and response
-- code. This calls `respond`.
textResponse' :: PathInfo r => Status -> Text -> Handler r
textResponse' status = respond status . fromByteString . encodeUtf8

-------------------------

-- |Produce a 200 OK response from the given UTF-8 text-generating
-- function. This calls `textUrlResponse'`.
textUrlResponse :: PathInfo r => (MkUrl r -> Text) -> Handler r
textUrlResponse = textUrlResponse' ok200

-- |Produce a response from the given UTF-8 text-generating function
-- and response code. This calls `respond`.
textUrlResponse' :: PathInfo r => Status -> (MkUrl r -> Text) -> Handler r
textUrlResponse' status text = do
  mkurl <- askMkUrl
  respond status . fromByteString . encodeUtf8 $ text mkurl

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
