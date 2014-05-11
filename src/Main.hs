{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Main where

import Browse.User
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (runReaderT)
import Database (migrateAll)
import Database.Persist (insert)
import Database.Persist.Sql (ConnectionPool, SqlPersistM, runSqlPersistMPool, runMigration)
import Database.Persist.Sqlite (withSqlitePool)
import Data.Text (Text, unpack)
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
          case args of
            ("runserver":_) -> runserver
            ("migrate":_)   -> migrate
            ("populate":_)  -> populate
            _ -> putStrLn "Invalid argument" >> exitFailure

-- |Run the server
-- Todo: have the host and port be parameters (config file?)
-- Todo: have the connection string be a parameter (config file?)
runserver :: IO ()
runserver = do putStrLn $ "Starting Λδ on port " ++ show (settingsPort defaultSettings)
               withPool "test.sqlite" (\pool -> runSettings defaultSettings $ lambdadelta pool)

-- |Migrate the database
-- Todo: have the connection string be a parameter (config file?)
migrate :: IO ()
migrate = withDB "test.sqlite" $ runMigration migrateAll

-- |Populate the database with test data
-- Todo: have the connection string be a parameter (config file?)
populate :: IO ()
populate = withDB "test.sqlite" $ do
             let board = D.Board "b" "Random" "Not like 4chan"
             void $ insert board

-------------------------

-- |Run a database function
withDB :: (MonadIO m, MonadBaseControl IO m)
       => Text          -- ^ The connection string
       -> SqlPersistM a -- ^ The database function
       -> m a
withDB connstr f = withPool connstr (\pool -> liftIO $ runSqlPersistMPool f pool)

-- |Run a database function which takes a connection pool
-- Todo: don't hard-code the database
-- Todo: size of database pool configurable
withPool :: (MonadIO m, MonadBaseControl IO m)
         => Text                   -- ^ The connection string
         -> (ConnectionPool -> m a) -- ^ The function
         -> m a
withPool connstr f = withSqlitePool connstr 10 f

-------------------------

-- |lambdadelta, or Λδ, is the actual WAI application. It takes a
-- request, handles it, and produces a response. This just consists of
-- checking the defined routes, checking for static files, and finally
-- failing with a 404 if nothing matches.
-- Todo: get the proper application root
lambdadelta :: ConnectionPool -> Application
lambdadelta = handleWai "http://localhost:3000" . routeRequest

-- |Route and process a request
-- Todo: use SqlPersistT?
routeRequest :: ConnectionPool -> MkUrl -> Sitemap -> Application
routeRequest pool mkurl path req = runSqlPersistMPool requestHandler pool
    where requestHandler = runReaderT (handler path) (mkurl, req)

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
-- Todo: get the proper filesystem root
static :: [Text] -- ^ The file path components
       -> Handler
static path = do let fullPath = joinPath $ "/tmp" : (map unpack path)
                 exists <- liftIO $ doesFileExist fullPath
                 return $ if exists
                          then responseFile ok200 [] fullPath Nothing
                          else responseLBS notFound404 [] "File not found"