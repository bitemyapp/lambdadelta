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
import Data.Text (Text)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Routes
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Types
import Web.Routes.PathInfo
import Web.Routes.Site

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
-- Todo: handle static files
lambdadelta :: ConnectionPool -> Application
lambdadelta pool req = case handle req of
                         Left _ -> return $ responseLBS notFound404 [] ""
                         Right resp -> runSqlPersistMPool resp pool
    where handle req = runSite "http://localhost:3000" (mkSitePI $ flip routeRequest req) $ pathInfo req

-- |The main router
-- Todo: would web-routes-wai be useful?
-- Todo: use SqlPersistT?
routeRequest :: MkUrl -> Request -> Sitemap -> SqlPersistM Response
routeRequest mkurl req Index           = runReaderT index (mkurl, req)
routeRequest mkurl req (Board b p)     = runReaderT (board b p) (mkurl, req)
routeRequest mkurl req (Thread b t)    = runReaderT (thread b t) (mkurl, req)
routeRequest mkurl req (PostThread b)  = runReaderT (postThread b) (mkurl, req)
routeRequest mkurl req (PostReply b t) = runReaderT (postReply b t) (mkurl, req)
