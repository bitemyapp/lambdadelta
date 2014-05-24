-- |Types used by request handlers.
module Web.Seacat.RequestHandler.Types where

import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.ConfigFile (ConfigParser)
import Data.Text (Text)
import Database.Persist.Sql (SqlPersistM)
import Network.Wai (Request, Response)
import Web.Routes.PathInfo (PathInfo)

-- |Function to make URLs from some routing type
type MkUrl r = r -> [(Text, Maybe Text)] -> Text

-- |Function which handles a request
type RequestProcessor r = ReaderT (ConfigParser, MkUrl r, Request) SqlPersistM

-- |`RequestProcessor` specialised to producing a `Response`. All
-- routes should go to a function of type `PathInfo r => Handler r`.
type Handler r = RequestProcessor r Response

-- |Get the configuration from a `RequestProcessor`
askConf :: PathInfo r => RequestProcessor r ConfigParser
askConf = asks $ \(conf, _, _) -> conf

-- |Get the URL maker from a `RequestProcessor`
askMkUrl :: PathInfo r => RequestProcessor r (MkUrl r)
askMkUrl = asks $ \(_, mkurl, _) -> mkurl

-- |Get the request from a `RequestProcessor`
askReq :: PathInfo r => RequestProcessor r Request
askReq = asks $ \(_, _, req) -> req
