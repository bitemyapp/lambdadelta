{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Main where

import Browse (respondFile)
import Browse.User
import Browse.Error (error404, error500)
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
main = do args <- getArgs
          let command = head args

          when (length args < 1) $
               putStrLn "Expected at least one argument" >> exitFailure

          config <- case args of
                     (_:conffile:_) -> loadConfigFile conffile
                     _ -> return $ Just defaults

          case config of
            Just conf -> run command conf
            Nothing -> putStrLn "Failed to read configuration" >> exitFailure

-------------------------

-- |Run one of the applications, depending on the command
run :: String -> CommandRunner
run "runserver" = runserver
run "migrate"   = migrate
run "populate"  = populate
run _           = badcommand

-- |Run the server
runserver :: CommandRunner
runserver conf = do let host = get' conf "server" "host"
                    let port = get' conf "server" "port"

                    let connstr  = get' conf "database" "connection_string"
                    let poolsize = get' conf "database" "pool_size"

                    let settings = setHost (fromString host) $
                                   setPort port
                                   defaultSettings

                    putStrLn $ "Starting Λδ on " ++ host ++ ":" ++ show port
                    withPool (fromString connstr) poolsize $
                        runSettings settings . lambdadelta conf

-- |Migrate the database
migrate :: CommandRunner
migrate conf = do let connstr  = get' conf "database" "connection_string"
                  let poolsize = get' conf "database" "pool_size"
                  withDB (fromString connstr) poolsize $ runMigration migrateAll

-- |Populate the database with test data
populate :: CommandRunner
populate conf = do let connstr  = get' conf "database" "connection_string"
                   let poolsize = get' conf "database" "pool_size"
                   withDB (fromString connstr) poolsize $ do
                       let board = D.Board "b" "Random" "Not like 4chan"
                       void $ insert board

-- |Fail with an error
badcommand :: CommandRunner
badcommand _ = putStrLn "Unknown command" >> exitFailure

-------------------------

-- |lambdadelta, or Λδ, is the actual WAI application. It takes a
-- request, handles it, and produces a response.
lambdadelta :: ConfigParser -> ConnectionPool -> Application
lambdadelta conf = let webroot = get' conf "server" "web_root"
                   in handleWai (fromString webroot) . routeRequest conf

-- |Route and process a request
-- Todo: use SqlPersistT?
routeRequest :: ConfigParser -> ConnectionPool -> MkUrl -> Sitemap -> Application
routeRequest conf pool mkurl path req = requestHandler `catchIOError` runError
    where requestHandler = runHandler $ handler path
          runError error = runHandler $ error500 (show error)
          runHandler h   = runPool (runReaderT h (conf, mkurl, req)) pool

-- |Route a request to a handler
handler :: Sitemap -> Handler
handler Index           = index
handler (Board b p)     = board b p
handler (Thread b t)    = thread b t
handler (PostThread b)  = postThread b
handler (PostReply b t) = postReply b t
handler Error404        = error404 "File not found"
handler path            = static $ toPathSegments path

-- |Process a request for a static file
-- This isn't the best way to serve static files, your actual web
-- server should really do this.
static :: [Text] -- ^ The file path components
       -> Handler
static path = respondFile . joinPath $ map unpack path
