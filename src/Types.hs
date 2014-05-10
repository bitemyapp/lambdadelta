{-# LANGUAGE Rank2Types #-}

module Types where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import Network.Wai (Request, Response)
import Routes (Sitemap)
import Text.Hamlet (HtmlUrl)

-- |Function to make URLs from a Sitemap
type MkUrl = Sitemap -> [(Text, Maybe Text)] -> Text

-- |Function which handles a request
type RequestProcessor r = MonadIO m => ReaderT (MkUrl, Request) m r
type Handler = RequestProcessor Response
