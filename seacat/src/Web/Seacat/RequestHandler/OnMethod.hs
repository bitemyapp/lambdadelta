{-# LANGUAGE OverloadedStrings #-}

-- |Higher-level handlers to restrict acecss based on HTTP method.
module Web.Seacat.RequestHandler.OnMethod ( on
                                          , onGet
                                          , onPost) where

import Control.Applicative ((<$>))
import Data.Either.Utils (forceEither)
import Network.HTTP.Types.Method (StdMethod(..), parseMethod)
import Network.Wai (requestMethod)
import Web.Routes.PathInfo (PathInfo)
import Web.Seacat.RequestHandler.Types (Handler, _req, askCry)

-- |Run the provided handler on a GET request
--
-- For example, you can use this as follows:
--
-- > let error405 = utf8Response methodNotAllowed405 "This route only works via GET"
-- > in error405 `onGet` doThings
onGet :: PathInfo r
      => Handler r -- ^ The error handler
      -> Handler r -- ^ The handler
      -> Handler r
onGet = on GET

-- |Run the provided handler on a Post request
onPost :: PathInfo r
       => Handler r -- ^ The error handler
       -> Handler r -- ^ The handler
       -> Handler r
onPost = on POST

-- |Run the provided handler if the HTTP method matches, running the
-- error handler if not. The error handler should respond with an HTTP
-- 405 code.
on :: PathInfo r
   => StdMethod -- ^ The method to run on
   -> Handler r -- ^ The handler to call if the method doesn't match.
   -> Handler r -- ^ The handler
   -> Handler r
on method on405 handler = do
  req <- _req <$> askCry
  let rmethod = forceEither . parseMethod . requestMethod $ req
  if rmethod == method
  then handler
  else on405
