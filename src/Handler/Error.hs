module Handler.Error ( error400
                     , error404
                     , error500) where

import Handler (htmlResponse)
import Network.HTTP.Types.Status ( Status
                                 , badRequest400
                                 , notFound404
                                 , internalServerError500)
import Routes (Sitemap)
import Types

import qualified Handler.Templates as T (error)

-- |Send a 400 error
error400 :: String -- ^ The error text
         -> Handler Sitemap
error400 = Handler.Error.error badRequest400

-- |Send a 404 error
error404 :: String -- ^ The error text
         -> Handler Sitemap
error404 = Handler.Error.error notFound404

-- |Send a 500 error (nto that these will ever happen, right?)
error500 :: String -- ^ The error text
         -> Handler Sitemap
error500 = Handler.Error.error internalServerError500

-------------------------

-- |Construct an error page
error :: Status  -- ^ The response code
      -> String  -- ^ A description of the error
      -> Handler Sitemap
error status description = htmlResponse status $ T.error status description
