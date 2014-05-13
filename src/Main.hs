{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.Text (Text, unpack)
import Database.Persist (insert)
import Database.Persist.Sql (SqlPersistM)
import Framework (runner)
import Handler (respondFile)
import System.FilePath.Posix (joinPath)
import Types (Handler)
import Web.Routes.PathInfo (toPathSegments)

-- Application imports
import Handler.User
import Handler.Error
import MyDatabase (migrateAll)
import Routes (Sitemap(..))

import qualified MyDatabase as D

main :: IO ()
main = runner route error500 migrateAll populate

-- |Populate the database with sample data
populate :: SqlPersistM ()
populate = do
  let board = D.Board "b" "Random" "Not like 4chan"
  void $ insert board

-- |Route a request to a handler
route :: Sitemap -> Handler Sitemap
route Index           = index
route (Board b p)     = board b p
route (Thread b t)    = thread b t
route (PostThread b)  = postThread b
route (PostReply b t) = postReply b t
route Error404        = error404 "File not found"
route path            = static (error404 "File not found") $ toPathSegments path

-- |Process a request for a static file
-- This isn't the best way to serve static files, your actual web
-- server should really do this.
static :: Handler Sitemap -- ^ 404 handler
       -> [Text]          -- ^ The file path components
       -> Handler Sitemap
static on404 path = respondFile on404 . joinPath $ map unpack path
