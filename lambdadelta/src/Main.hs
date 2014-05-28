{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.ConfigFile (ConfigParser, emptyCP, readstring)
import Data.Either.Utils (forceEither)
import Database (migrateAll)
import Database.Persist (insert)
import Database.Persist.Sql (SqlPersistM)
import Handler.Error
import Handler.File
import Handler.User
import Network.HTTP.Types.Method (StdMethod(..))
import Routes (Sitemap(..))
import Web.Seacat
import Web.Seacat.RequestHandler.OnMethod (onPost)
import Web.Seacat.RequestHandler.Types (Handler)

import qualified Database as D

main :: IO ()
main = seacat route error500 $ defaultSettings { _config = Just defaults
                                               , _migrate = Just migrateAll
                                               , _populate = Just populate
                                               }

-- |Populate the database with sample data
populate :: SqlPersistM ()
populate = do
  let board = D.Board "b" "Random" "Not like 4chan"
  void $ insert board

-- |Route a request to a handler
-- These pattern matches must cover all cases, as otherwise an
-- internal server error might be raised in Seacat
route :: StdMethod -> Sitemap -> Handler Sitemap
route GET Index           = index
route GET (Board b p)     = board b p
route GET (Thread b t)    = thread b t
route GET Banner          = banner
route GET Error404        = error404 "File not found"
route _   (PostThread b)  = error405 "Method not allowed" `onPost` postThread b
route _   (PostReply b t) = error405 "Method not allowed" `onPost` postReply b t
route _ _                  = error405 "Method not allowed"

-- |Default configuration values
defaults :: ConfigParser
defaults = forceEither . readstring emptyCP $ unlines
  [ "[database]"
  , "connection_string = lambdadelta.sqlite"
  , "[board]"
  , "board_listing = [[\"b\"]]"
  , "summary_size = 5"
  , "threads_per_page = 10"
  , "maximum_pages = 10"
  , "thumbnail_width = 300"
  , "thumbnail_height = 300"
  , "bump_limit = 300"
  , "banner_dir = banners"
  , "allow_sage = true"
  , "allow_noko = true"
  , "always_noko = false"
  ]
