{-# LANGUAGE OverloadedStrings #-}

module Web.Seacat.RequestHandler.OnMethod ( on
                                          , onGet
                                          , onPost) where

import Data.Either.Utils (forceEither)
import Network.HTTP.Types.Method (StdMethod(..), parseMethod)
import Network.Wai (requestMethod)
import Web.Routes.PathInfo (PathInfo)
import Web.Seacat.RequestHandler.Types (Handler, askReq)

-- |Run the provided handler on a GET request
onGet :: PathInfo r
      => Handler r -- ^ The 405 handler
      -> Handler r -- ^ The handler
      -> Handler r
onGet = on GET

-- |Run the provided handler on a Post request
onPost :: PathInfo r
       => Handler r -- ^ The 405 handler
       -> Handler r -- ^ The handler
       -> Handler r
onPost = on POST

-- |Run the provided handler if the HTTP method matches, running the
-- 405 error handler if not.
on :: PathInfo r
   => StdMethod -- ^ The method to run on
   -> Handler r -- ^ The 405 handler
   -> Handler r -- ^ The handler
   -> Handler r
on method on405 handler = do
  req <- askReq
  let rmethod = forceEither . parseMethod . requestMethod $ req
  if rmethod == method
  then handler
  else on405
