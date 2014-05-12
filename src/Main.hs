{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Main where

import Handler (respondFile)
import Handler.User
import Handler.Error (error404, error500)
import Configuration (ConfigParser, loadConfigFile, defaults, get')
import Control.Exception.Base ()
import Control.Monad (when, void)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString ()
import Data.String (fromString)
import Data.Text (Text, unpack)
import Database (migrateAll, withDB, withPool, runPool)
import Database.Persist (insert)
import Database.Persist.Sql (ConnectionPool, runMigration)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (setHost, setPort, defaultSettings, runSettings)
import Routes
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath.Posix (joinPath)
import System.IO.Error (catchIOError)
import Types
import Web.Routes.Wai (handleWai)
import Web.Routes.PathInfo (toPathSegments)

import qualified Database as D

-- |Fire up the appropriate process, depending on the command.
main :: IO ()
main = do
  args <- getArgs

  when (length args < 1) $
    die "Expected at least one argument"

  let command = head args


  config <- case args of
             (_:conffile:_) -> loadConfigFile conffile
             _ -> return $ Just defaults

  case config of
    Just conf -> run command conf
    Nothing   -> die "Failed to read configuration"

-- |Die with a fatal error
die :: String -- ^ The error description
    -> IO ()
die err = putStrLn err >> exitFailure

-------------------------

-- |Run one of the applications, depending on the command
run :: String -> CommandRunner
run "runserver" = runserver
run "migrate"   = migrate
run "populate"  = populate
run _           = badcommand

-- |Run the server
runserver :: CommandRunner
runserver conf = do
  let host = get' conf "server" "host"
  let port = get' conf "server" "port"

  let connstr  = get' conf "database" "connection_string"
  let poolsize = get' conf "database" "pool_size"

  let settings = setHost (fromString host) . setPort port $ defaultSettings

  putStrLn $ "Starting Λδ on " ++ host ++ ":" ++ show port
  withPool (fromString connstr) poolsize $
    runSettings settings . lambdadelta conf

-- |Migrate the database
migrate :: CommandRunner
migrate conf = do
  let connstr  = get' conf "database" "connection_string"
  let poolsize = get' conf "database" "pool_size"
  withDB (fromString connstr) poolsize $ runMigration migrateAll

-- |Populate the database with test data
populate :: CommandRunner
populate conf = do
  let connstr  = get' conf "database" "connection_string"
  let poolsize = get' conf "database" "pool_size"
  withDB (fromString connstr) poolsize $ do
    let board = D.Board "b" "Random" "Not like 4chan"
    void $ insert board

-- |Fail with an error
badcommand :: CommandRunner
badcommand _ = die "Unknown command"

-------------------------

-- |lambdadelta, or Λδ, is the actual WAI application. It takes a
-- request, handles it, and produces a response.
lambdadelta :: ConfigParser -> ConnectionPool -> Application
lambdadelta conf = let webroot = get' conf "server" "web_root"
                   in handleWai (fromString webroot) . process conf

-- |Route and process a request
-- Todo: use SqlPersistT?
process :: ConfigParser -> ConnectionPool -> MkUrl -> Sitemap -> Application
process conf pool mkurl path req = requestHandler `catchIOError` runError
  where requestHandler = runHandler $ route path
        runError err   = runHandler $ error500 (show err)
        runHandler h   = runPool (runReaderT h (conf, mkurl, req)) pool

-- |Route a request to a handler
route :: Sitemap -> Handler
route Index           = index
route (Board b p)     = board b p
route (Thread b t)    = thread b t
route (PostThread b)  = postThread b
route (PostReply b t) = postReply b t
route Error404        = error404 "File not found"
route path            = static $ toPathSegments path

-- |Process a request for a static file
-- This isn't the best way to serve static files, your actual web
-- server should really do this.
static :: [Text] -- ^ The file path components
       -> Handler
static path = respondFile . joinPath $ map unpack path
