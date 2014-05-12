{-# LANGUAGE Rank2Types #-}

module Types where

import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.ConfigFile (ConfigParser)
import Data.Text (Text)
import Database.Persist.Sql (SqlPersistM)
import Network.Wai (Request, Response)
import Routes (Sitemap)

-- |Function to make URLs from a Sitemap
type MkUrl = Sitemap -> [(Text, Maybe Text)] -> Text

-- |Function which handles a request
-- Todo: Should probably use SqlPersistT
type RequestProcessor = ReaderT (ConfigParser, MkUrl, Request) SqlPersistM
type Handler = RequestProcessor Response

-- |Function which runs an IO command (eg, runserver)
type CommandRunner = ConfigParser -> IO ()

-- |Get the configuration from a RequestProcessor
askConf :: RequestProcessor ConfigParser
askConf = asks $ \(conf, _, _) -> conf

-- |Get the URL maker from a RequestProcessor
askMkUrl :: RequestProcessor MkUrl
askMkUrl = asks $ \(_, mkurl, _) -> mkurl

-- |Get the request from a RequestProcessor
askReq :: RequestProcessor Request
askReq = asks $ \(_, _, req) -> req
