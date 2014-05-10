{-# LANGUAGE OverloadedStrings #-}

module Main where

import Browse.User
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Maybe
import Data.Text (Text)
import Database.Persist.Sqlite
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
          runSettings defaultSettings lambdadelta

-- |lambdadelta, or Λδ, is the actual WAI application. It takes a
-- request, handles it, and produces a response. This just consists of
-- checking the defined routes, checking for static files, and finally
-- failing with a 404 if nothing matches.
-- Todo: get the proper application root
-- Todo: handle static files
-- Todo: don't hard-code the database
-- Todo: size of database pool configurable
lambdadelta :: Application
lambdadelta req = withSqlitePool ":memory:" 10 $ \pool ->
    case runSite "/" (mkSitePI $ flip routeRequest req) $ pathInfo req of
      Left _ -> return $ responseLBS notFound404 [] ""
      Right resp -> liftIO $ runSqlPersistMPool resp pool

-- |The main router
-- Todo: would web-routes-wai be useful?
routeRequest :: MonadIO m => MkUrl -> Request -> Sitemap -> m Response
routeRequest mkurl req Index           = runReaderT index (mkurl, req)
routeRequest mkurl req (Board b p)     = runReaderT (board b p) (mkurl, req)
routeRequest mkurl req (Thread b t)    = runReaderT (thread b t) (mkurl, req)
routeRequest mkurl req (PostThread b)  = runReaderT (postThread b) (mkurl, req)
routeRequest mkurl req (PostReply b t) = runReaderT (postReply b t) (mkurl, req)
