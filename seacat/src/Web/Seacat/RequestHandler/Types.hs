-- |Types used by request handlers.
module Web.Seacat.RequestHandler.Types where

import Control.Applicative((<$>))
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ConfigFile (ConfigParser)
import Data.Text (Text)
import Database.Persist.Sql (SqlPersistM)
import Network.Wai (Request, Response)
import Network.Wai.Parse (FileInfo)
import Web.Routes.PathInfo (PathInfo)

-- |Type to represent a Seacat request
data Cry = Cry { _req    :: Request
               -- ^ The underlying WAI request.

               , _params :: [(Text, Text)]
               -- ^ The parameters, parsed once before the top-level
               -- handler is called. This is because you can't query
               -- the parameters of a `Request` multiple times, as
               -- it's implemented as an IO Conduit source.

               , _files  :: [(Text, FileInfo ByteString)]
               -- ^ The files, stored in memory as lazy bytestrings,
               -- and parsed out of the request once for the same
               -- reasoning as _params.
               }

-- |Function to make URLs from some routing type
type MkUrl r = r -> [(Text, Maybe Text)] -> Text

-- |Function which handles a request
type RequestProcessor r = ReaderT (ConfigParser, MkUrl r, Cry) SqlPersistM

-- |`RequestProcessor` specialised to producing a `Response`. All
-- routes should go to a function of type `PathInfo r => Handler r`.
type Handler r = RequestProcessor r Response

-- |Get the configuration from a `RequestProcessor`
askConf :: PathInfo r => RequestProcessor r ConfigParser
askConf = asks $ \(conf, _, _) -> conf

-- |Get the URL maker from a `RequestProcessor`
askMkUrl :: PathInfo r => RequestProcessor r (MkUrl r)
askMkUrl = asks $ \(_, mkurl, _) -> mkurl

-- |Get the seacat request from a `RequestProcessor`
askCry :: PathInfo r => RequestProcessor r Cry
askCry = asks $ \(_, _, req) -> req

-- |Get the WAI request from a `RequestProcessor`
askReq :: PathInfo r => RequestProcessor r Request
askReq = _req <$> askCry
