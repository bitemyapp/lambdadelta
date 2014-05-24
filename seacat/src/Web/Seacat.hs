{-# LANGUAGE OverloadedStrings #-}

-- |The entry point of the Seacat module hierarchy: in this module
-- lives everyting related to starting up an application (done under
-- the hood with Warp and WAI), and submodules of this comprise all
-- Seacat functionality.
module Web.Seacat ( SeacatSettings(..)
                  , defaultSettings
                  , seacat) where

import Control.Monad (when)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Either.Utils (forceEither)
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text (replace)
import Database.Persist.Sql (ConnectionPool, Migration, SqlPersistM, runMigration)
import Network.HTTP.Types.Method (StdMethod(..), parseMethod)
import Network.Wai (Application, requestMethod)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO.Error (catchIOError)
import Web.Routes.PathInfo (PathInfo)
import Web.Routes.Wai (handleWai)

import Web.Seacat.Configuration (ConfigParser, applyUserConfig, loadConfigFile, reloadConfigFile, defaults, get')
import Web.Seacat.Database (runPool, withPool, withDB, migrateAll)
import Web.Seacat.RequestHandler.Types (Handler, MkUrl)

import qualified Network.Wai.Handler.Warp as W

-- |Optional configuration for Seacat servers
data SeacatSettings = SeacatSettings
    { _config   :: Maybe ConfigParser
      -- ^ Default configuration, overriding the defaults. This should
      -- include all application-required configuration not already
      -- provided by Seacat. Configuration is applied as follows,
      -- where \`merge\` overrides the values in its first argument by
      -- the values in its second,
      --
      --     seacat defaults \`merge\` application config \`merge\` user config
      --
      -- This ensures that all the required configuration values are set.

    , _migrate  :: Maybe (Migration SqlPersistM)
      -- ^ Database migration handler. If a database is used, this must
      -- be provided (or migrations handled manually in runserver).

    , _populate :: Maybe (SqlPersistM ())
      -- ^ Database population handler. If a database is used, this
      -- must be provided (or population handled manually in
      -- runserver).
    }

-- |Default configuration: no application-specific configuration, no
-- migration handler, and no population handler.
defaultSettings :: SeacatSettings
defaultSettings = SeacatSettings { _config   = Nothing
                                 , _migrate  = Nothing
                                 , _populate = Nothing
                                 }

-- |Launch the Seacat web server. Seacat takes two bits of mandatory
-- configuration, a routing function and a 500 handler, and then some
-- optional configuration. By default, the server listens on *:3000.
seacat :: PathInfo r
       => (StdMethod -> r -> Handler r) -- ^ Routing function
       -> (String -> Handler r) -- ^ Top-level error handling function
       -> SeacatSettings -- ^ Optional configuration
       -> IO ()
seacat route on500 settings = do
  args <- getArgs

  when (length args < 1) $
    die "Expected at least one argument"

  let command = head args

  let confFile = case args of
                   (_:conffile:_) -> Just conffile
                   _ -> Nothing

  config <- case confFile of
             Just cfile -> loadConfigFile cfile
             Nothing -> return $ Just defaults

  case config of
    Just conf -> run command route on500 confFile $ settings { _config = Just $ applyUserConfig conf (_config settings) }
    Nothing   -> die "Failed to read configuration"

-- |Die with a fatal error
die :: String -- ^ The error description
    -> IO ()
die err = putStrLn err >> exitFailure

-------------------------

-- |Run one of the applications, depending on the command
run :: PathInfo r
    => String -- ^ Command
    -> (StdMethod -> r -> Handler r) -- ^ Routing function
    -> (String -> Handler r) -- ^ Top-level error handling function
    -> Maybe FilePath -- ^ The config file
    -> SeacatSettings -- ^ The optional settings
    -> IO ()
run "runserver" = runserver
run "migrate"   = migrate
run "populate"  = populate
run _           = badcommand

-- |Run the server
runserver :: PathInfo r
          => (StdMethod -> r -> Handler r)
          -> (String -> Handler r)
          -> Maybe FilePath
          -> SeacatSettings
          -> IO ()
runserver route on500 cfile settings = do
  let conf = fromJust $ _config settings

  let host = get' conf "server" "host"
  let port = get' conf "server" "port"

  let backend  = get' conf "database" "backend"
  let connstr  = get' conf "database" "connection_string" 
  let poolsize = get' conf "database" "pool_size"

  let settings = setHost (fromString host) . setPort port $ W.defaultSettings

  putStrLn $ "Starting Seacat on " ++ host ++ ":" ++ show port
  withPool (fromString backend) (fromString connstr) poolsize $
    runSettings settings . runner route on500 (conf,cfile)

-- |Migrate the database
migrate :: PathInfo r
        => (StdMethod -> r -> Handler r)
        -> (String -> Handler r)
        -> Maybe FilePath
        -> SeacatSettings
        -> IO ()
migrate _ _ _ settings = do
  let conf = fromJust $ _config settings
  let backend  = get' conf "database" "backend"
  let connstr  = get' conf "database" "connection_string"
  let poolsize = get' conf "database" "pool_size"
  withDB (fromString backend) (fromString connstr) poolsize $ runMigration migrateAll

  case _migrate settings of
    Just migration -> do
      withDB (fromString backend) (fromString connstr) poolsize $ runMigration migration
    Nothing -> putStrLn "No application migration handler."

-- |Populate the database with test data
populate :: PathInfo r
         => (StdMethod -> r -> Handler r)
         -> (String -> Handler r)
         -> Maybe FilePath
         -> SeacatSettings
         -> IO ()
populate _ _ _ settings =
    case _populate settings of
      Just pop -> do
        let conf = fromJust $ _config settings
        let backend  = get' conf "database" "backend"
        let connstr  = get' conf "database" "connection_string"
        let poolsize = get' conf "database" "pool_size"
        withDB (fromString backend) (fromString connstr) poolsize pop

      Nothing -> die "No population handler."

-- |Fail with an error
badcommand :: PathInfo r
           => (StdMethod -> r -> Handler r)
           -> (String -> Handler r)
           -> Maybe FilePath
           -> SeacatSettings
           -> IO ()
badcommand _ _ _ _ = die "Unknown command"

-------------------------

-- |runner is the actual WAI application. It takes a request, handles
-- it, and produces a response.
runner :: PathInfo r
       => (StdMethod -> r -> Handler r)    -- ^ Routing function
       -> (String -> Handler r)           -- ^ Top-level error handling function
       -> (ConfigParser, Maybe FilePath) -- ^ The configuration
       -> ConnectionPool                 -- ^ Database connection reference
       -> Application
runner route on500 c@(conf,_) = let webroot = get' conf "server" "web_root"
                                in handleWai (fromString webroot) . process route on500 c

-- |Route and process a request
-- Todo: use SqlPersistT?
process :: PathInfo r
        => (StdMethod -> r -> Handler r) -- ^ Routing function
        -> (String -> Handler r)        -- ^ Top-level error handling function
        -> (ConfigParser, Maybe FilePath) -- ^ The configuration
        -> ConnectionPool              -- ^ Database connection reference
        -> MkUrl r                     -- ^ URL building function
        -> r                           -- ^ Requested route
        -> Application
process route on500 (conf,cfile) pool mkurl path req = requestHandler `catchIOError` runError
  where requestHandler = runHandler $ route method path
        runError err   = runHandler $ on500 (show err)
        runHandler h   = do
          conf' <- case cfile of
                    Just cf -> reloadConfigFile conf cf
                    Nothing -> return conf
          runPool (runReaderT h (conf', mkurl', req)) pool
        method         = forceEither . parseMethod . requestMethod $ req
        mkurl' r args  = replace "%23" "#" $ mkurl r args
