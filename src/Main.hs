{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Main where

import Browse.User
import Configuration (defaults, loadConfigFile)
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString ()
import Data.ConfigFile (ConfigParser, get)
import Data.Either.Utils (forceEither)
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text (Text, unpack)
import Database (migrateAll)
import Database.Persist (insert)
import Database.Persist.Sql (ConnectionPool, SqlPersistM, runSqlPersistMPool, runMigration)
import Database.Persist.Sqlite (withSqlitePool)
import Network.HTTP.Types.Status (ok200, notFound404)
import Network.Wai
import Network.Wai.Handler.Warp
import Routes
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.FilePath.Posix (joinPath)
import Types
import Web.Routes.Wai (handleWai)
import Web.Routes.PathInfo (toPathSegments)

import qualified Database as D

-- |Fire up the server on the default port and just listen forever for requests.
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
runserver conf = do let host = forceEither $ get conf "server" "host"
                    let port = forceEither $ get conf "server" "port"

                    let connstr  = forceEither $ get conf "database" "connection_string"
                    let poolsize = forceEither $ get conf "database" "pool_size"

                    let settings = setHost (fromString host) $
                                   setPort port
                                   defaultSettings

                    putStrLn $ "Starting Λδ on " ++ host ++ ":" ++ show port
                    withPool (fromString connstr) poolsize $
                        \pool -> runSettings settings $ lambdadelta conf pool

-- |Migrate the database
migrate :: ConfigParser -> IO ()
migrate conf = do let connstr  = forceEither $ get conf "database" "connection_string"
                  let poolsize = forceEither $ get conf "database" "pool_size"
                  withDB (fromString connstr) poolsize $ runMigration migrateAll

-- |Populate the database with test data
populate :: ConfigParser -> IO ()
populate conf = do let connstr  = forceEither $ get conf "database" "connection_string"
                   let poolsize = forceEither $ get conf "database" "pool_size"
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
lambdadelta conf = let webroot = forceEither $ get conf "server" "web_root"
                   in handleWai (fromString webroot) . routeRequest conf

-- |Route and process a request
-- Todo: use SqlPersistT?
routeRequest :: ConfigParser -> ConnectionPool -> MkUrl -> Sitemap -> Application
routeRequest conf pool mkurl path req = runSqlPersistMPool requestHandler pool
    where requestHandler = runReaderT (handler path conf) (mkurl, req)

-- |Route a request to a handler
handler :: Sitemap -> Handler
handler Index           = index
handler (Board b p)     = board b p
handler (Thread b t)    = thread b t
handler (PostThread b)  = postThread b
handler (PostReply b t) = postReply b t
handler path            = static $ toPathSegments path

-- |Process a request for a static file
-- This isn't the best way to serve static files, your actual web
-- server should really do this.
static :: [Text] -- ^ The file path components
       -> Handler
static path conf = do let fileroot = forceEither $ get conf "server" "file_root"
                      let fullPath = joinPath $ fileroot : map unpack path

                      exists <- liftIO $ doesFileExist fullPath
                      return $ if exists
                               then responseFile ok200 [] fullPath Nothing
                               else responseLBS notFound404 [] "File not found"