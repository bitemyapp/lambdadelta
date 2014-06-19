-- |This is a port of (what I use of) Web.Routes.Wai to Wai 3.0, as I
-- got tired of waiting for upstream to update.
module Web.Seacat.Router ( handleWai
                         , handleWai_
                         , handleWaiError ) where

import Data.Text (Text, append)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (status404)
import Network.Wai (Application, rawPathInfo , responseLBS)
import Web.Routes.PathInfo (PathInfo(..), fromPathInfo, toPathInfoParams, stripOverlapBS)

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L

-- | A URL pretty printer. This is like MkUrl from
-- Web.Seacat.RequestHandler.Types, except that it uses a (Text, Maybe
-- Text) to stick close to web-routes-wai, easing migration back when
-- it is finally updated.
type MkUrl r = r -> [(Text, Maybe Text)] -> Text

-- | A URL parser. This turns a URL into either an error message, or a
-- parsed URL type, suitable for giving to the routing function.
type FromUrl r = C.ByteString -> Either String r

-- | A routing function type.
-- 
-- Takes a URL printing function and a parsed URL and returns the
-- appropriate Application for that URL.
type RoutingFunction r = MkUrl r -> r -> Application

-- |Convert a routing function into an Application by using PathInfo.
handleWai :: PathInfo r
          => C.ByteString -- ^ The application web route
          -> RoutingFunction r
          -> Application
handleWai = handleWai_ toPathInfoParams fromPathInfo

-------------------------

-- |Convert a URL parser, a URL printer, and a routing function into
-- an Application. If the URL parsing fails, this returns a 404.
handleWai_ :: MkUrl r
           -> FromUrl r
           -> C.ByteString -- ^ The application web route
           -> RoutingFunction r
           -> Application
handleWai_ fromUrl toUrl approot handler =
    handleWaiError fromUrl toUrl approot handleError handler
    where
      handleError parseError  = \wreq receiver -> handleError' parseError wreq >>= receiver
      handleError' parseError = \_ -> return $ responseLBS status404 [] (L.pack parseError)

-------------------------

-- |Convert a URL parser, a URL printer, an error handling function,
-- and a routing function into an application.
handleWaiError :: MkUrl r
               -> FromUrl r
               -> C.ByteString -- ^ The application web route
               -> (String -> Application) -- ^ Error handling function
               -> RoutingFunction r
               -> Application
handleWaiError fromUrl toUrl approot handleError handler = \req receiver -> do
  let fUrl = toUrl $ stripOverlapBS approot $ rawPathInfo req
  case fUrl of
    Left err  -> handleError err req receiver
    Right url -> handler (\url params -> (decodeUtf8 approot) `append` (fromUrl url params)) url req receiver
