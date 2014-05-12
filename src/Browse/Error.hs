module Browse.Error ( error400
                    , error404
                    , error500) where

import Browse (htmlResponse)
import Network.HTTP.Types.Status ( Status
                                 , badRequest400
                                 , notFound404
                                 , internalServerError500)
import Types

import qualified Browse.Templates as T (error)

-- |Send a 400 error
error400 :: String -- ^ The error text
         -> Handler
error400 = Browse.Error.error badRequest400

-- |Send a 404 error
error404 :: String -- ^ The error text
         -> Handler
error404 = Browse.Error.error notFound404

-- |Send a 500 error (nto that these will ever happen, right?)
error500 :: String -- ^ The error text
         -> Handler
error500 = Browse.Error.error internalServerError500

-------------------------

-- |Construct an error page
error :: Status -> String -> Handler
error status description = htmlResponse status $ T.error status description
