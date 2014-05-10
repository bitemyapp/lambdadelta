{-# LANGUAGE Rank2Types #-}

module Types where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Network.Wai (Request, Response)
import Routes (Sitemap)

-- |Function to make URLs from a Sitemap
type MkUrl = Sitemap -> [(Text, Maybe Text)] -> Text

-- |Function which handles a request
type Handler = MonadIO m => MkUrl -> Request -> m Response
