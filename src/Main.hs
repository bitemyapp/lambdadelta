{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Main where

import Browse (respondFile)
import Browse.User
import Browse.Error (error404, error500)
import Configuration (loadConfigFile, defaults, get')
import Control.Exception.Base ()
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString ()
import Data.ConfigFile (ConfigParser)
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text (Text, unpack)
import Database (migrateAll)
import Database.Persist (insert)
import Database.Persist.Sql (ConnectionPool, SqlPersistM, runSqlPersistMPool, runMigration)
import Database.Persist.Sqlite (withSqlitePool)
import Network.Wai (Application)
import Network.Wai.Handler.Warp
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

          when (length args < 1) $
               putStrLn "Expected at least one argument" >> exitFailure

          config <- case args of
                     (_:conffile:_) -> loadConfigFile conffile
                     _ -> return $ Just defaults

          case config of
            Nothing -> putStrLn "Failed to read configuration" >> exitFailure
            _ -> return ()

          let command = head args

          case command of
            "runserver" -> runserver $ fromJust config
            "migrate"   -> migrate   $ fromJust config
            "populate"  -> populate  $ fromJust config
            _ -> putStrLn "Unknown command" >> exitFailure

-- |Run the server
runserver :: ConfigParser -> IO ()
runserver conf = do let host = get' conf "server" "host"
                    let port = get' conf "server" "port"

                    let connstr  = get' conf "database" "connection_string"
                    let poolsize = get' conf "database" "pool_size"

                    let settings = setHost (fromString host) $
                                   setPort port
                                   defaultSettings

                    putStrLn $ "Starting Λδ on " ++ host ++ ":" ++ show port
                    withPool (fromString connstr) poolsize $
                        \pool -> runSettings settings $ lambdadelta conf pool

-- |Migrate the database
migrate :: ConfigParser -> IO ()
migrate conf = do let connstr  = get' conf "database" "connection_string"
                  let poolsize = get' conf "database" "pool_size"
                  withDB (fromString connstr) poolsize $ runMigration migrateAll

-- |Populate the database with test data
populate :: ConfigParser -> IO ()
populate conf = do let connstr  = get' conf "database" "connection_string"
                   let poolsize = get' conf "database" "pool_size"
                   withDB (fromString connstr) poolsize $ do
                       let board = D.Board "b" "Random" "Not like 4chan"
                       void $ insert board

-------------------------

-- |Run a database function
withDB :: (MonadIO m, MonadBaseControl IO m)
       => Text          -- ^ The connection string
       -> Int           -- ^ The pool size
       -> SqlPersistM a -- ^ The database function
       -> m a
withDB connstr psize f = withPool connstr psize $ liftIO . runSqlPersistMPool f

-- |Run a database function which takes a connection pool
withPool :: (MonadIO m, MonadBaseControl IO m)
         => Text                   -- ^ The connection string
         -> Int                    -- ^ The pool size
         -> (ConnectionPool -> m a) -- ^ The function
         -> m a
withPool = withSqlitePool

-------------------------

-- |lambdadelta, or Λδ, is the actual WAI application. It takes a
-- request, handles it, and produces a response. This just consists of
-- checking the defined routes, checking for static files, and finally
-- failing with a 404 if nothing matches.
lambdadelta :: ConfigParser -> ConnectionPool -> Application
lambdadelta conf = let webroot = get' conf "server" "web_root"
                   in handleWai (fromString webroot) . routeRequest conf

-- |Route and process a request
-- Todo: use SqlPersistT?
routeRequest :: ConfigParser -> ConnectionPool -> MkUrl -> Sitemap -> Application
routeRequest conf pool mkurl path req = runSqlPersistMPool requestHandler pool `catchIOError` \error -> runSqlPersistMPool (runError error) pool
    where requestHandler = runHandler $ handler path
          runError error = runHandler $ error500 (show error)
          runHandler h   = runReaderT h (conf, mkurl, req)

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
