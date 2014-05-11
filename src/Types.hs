{-# LANGUAGE Rank2Types #-}

module Types where

import Control.Monad.Trans.Reader (ReaderT)
import Data.ConfigFile (ConfigParser)
import Data.Text (Text)
import Database.Persist.Sql (SqlPersistM)
import Network.Wai (Request, Response)
import Routes (Sitemap)

-- |Function to make URLs from a Sitemap
type MkUrl = Sitemap -> [(Text, Maybe Text)] -> Text

-- |Function which handles a request
-- Todo: Should probably use SqlPersistT
type RequestProcessor r = ReaderT (MkUrl, Request) SqlPersistM r
type Handler = ConfigParser -> RequestProcessor Response
