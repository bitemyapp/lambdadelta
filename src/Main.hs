{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Main where

import Browse.User
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (runReaderT)
import Database (migrateAll)
import Database.Persist.Sql (ConnectionPool, SqlPersistM, runSqlPersistMPool, runMigration)
import Database.Persist.Sqlite (withSqlitePool)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Routes
import Types
import Web.Routes.PathInfo
import Web.Routes.Site

-- |Fire up the server on the default port and just listen forever for requests.
-- Todo: have the host and posrt be parameters
-- Todo: have different execution modes (run server, migrate database, etc)
main :: IO ()
main = do putStrLn $ "Starting Λδ on port " ++ show (settingsPort defaultSettings)
          withDB $ runMigration migrateAll
          withPool (\pool -> runSettings defaultSettings $ lambdadelta pool)


-- |Run a database function
withDB :: (MonadIO m, MonadBaseControl IO m) => SqlPersistM a -> m a
withDB f = withPool (\pool -> liftIO $ runSqlPersistMPool f pool)

-- |Run a database function which takes a connection pool
-- Todo: don't hard-code the database
-- Todo: size of database pool configurable
withPool :: (MonadIO m, MonadBaseControl IO m) => (ConnectionPool -> m a) -> m a
withPool f = withSqlitePool "test.sqlite" 10 f

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
    where handle req = runSite "/" (mkSitePI $ flip routeRequest req) $ pathInfo req

-- |The main router
-- Todo: would web-routes-wai be useful?
-- Todo: use SqlPersistT?
routeRequest :: MkUrl -> Request -> Sitemap -> SqlPersistM Response
routeRequest mkurl req Index           = runReaderT index (mkurl, req)
routeRequest mkurl req (Board b p)     = runReaderT (board b p) (mkurl, req)
routeRequest mkurl req (Thread b t)    = runReaderT (thread b t) (mkurl, req)
routeRequest mkurl req (PostThread b)  = runReaderT (postThread b) (mkurl, req)
routeRequest mkurl req (PostReply b t) = runReaderT (postReply b t) (mkurl, req)
