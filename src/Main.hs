{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Data.Text

import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp

import Routes

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
lambdadelta :: Application
lambdadelta req = return $
    case runSite "/" (mkSitePI $ flip routeRequest req) $ pathInfo req of
      Left _ -> responseLBS notFound404 [] ""
      Right resp -> resp
