module Web.Seacat.Types where

import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.ConfigFile (ConfigParser)
import Data.Text (Text)
import Database.Persist.Sql (SqlPersistM)
import Network.Wai (Request, Response)
import Web.Routes.PathInfo (PathInfo)

-- |Function to make URLs from some routing type
type MkUrl r = r -> [(Text, Maybe Text)] -> Text

-- |Function which handles a request
-- Todo: Should probably use SqlPersistT
type RequestProcessor r = ReaderT (ConfigParser, MkUrl r, Request) SqlPersistM
type Handler r = RequestProcessor r Response

-- |Function which runs an IO command (eg, runserver)
type CommandRunner = ConfigParser -> IO ()

-- |Get the configuration from a RequestProcessor
askConf :: PathInfo r => RequestProcessor r ConfigParser
askConf = asks $ \(conf, _, _) -> conf

-- |Get the URL maker from a RequestProcessor
askMkUrl :: PathInfo r => RequestProcessor r (MkUrl r)
askMkUrl = asks $ \(_, mkurl, _) -> mkurl

-- |Get the request from a RequestProcessor
askReq :: PathInfo r => RequestProcessor r Request
askReq = asks $ \(_, _, req) -> req
