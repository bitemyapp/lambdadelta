{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.ConfigFile (ConfigParser, emptyCP, readstring)
import Data.Either.Utils (forceEither)
import Database (migrateAll)
import Database.Persist (insert)
import Database.Persist.Sql (SqlPersistM)
import Handler.Error
import Handler.User
import Handler.File
import Routes (Sitemap(..))
import Web.Routes.PathInfo (toPathSegments)
import Web.Seacat (seacat')
import Web.Seacat.Types (Handler)

import qualified Database as D

main :: IO ()
main = seacat' defaults route error500 migrateAll populate

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
route Banner          = banner
route Error404        = error404 "File not found"
route path            = static (error404 "File not found") $ toPathSegments path

-- |Default configuration values
defaults :: ConfigParser
defaults = forceEither . readstring emptyCP $ unlines
  [ "[server]"
  , "host      = *"
  , "port      = 3000"
  , "web_root  = http://localhost:3000"
  , "file_root = /tmp"
  , "[database]"
  , "connection_string = lambdadelta.sqlite"
  , "pool_size         = 10"
  , "[board]"
  , "board_listing = [[\"b\"]]"
  , "summary_size = 5"
  , "threads_per_page = 10"
  , "maximum_pages = 10"
  , "thumbnail_width = 300"
  , "thumbnail_height = 300"
  , "bump_limit = 300"
  , "banner_dir = banners"
  ]
