{-# LANGUAGE OverloadedStrings #-}

module Web.Seacat (seacat, seacat') where

import Control.Monad (when)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Either.Utils (forceEither)
import Data.Text (replace)
import Data.String (fromString)
import Database.Persist.Sql (ConnectionPool, Migration, SqlPersistM, runMigration)
import Network.HTTP.Types.Method (StdMethod(..), parseMethod)
import Network.Wai (Application, requestMethod)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO.Error (catchIOError)
import Web.Routes.PathInfo (PathInfo)
import Web.Routes.Wai (handleWai)

import Web.Seacat.Configuration (ConfigParser, applyUserConfig, loadConfigFile, reloadConfigFile, defaults, get')
import Web.Seacat.Database (runPool, withPool, withDB)
import Web.Seacat.Database.Internal (migrateAll)
import Web.Seacat.RequestHandler.Types (Handler, MkUrl)

-- |Wrapper for seacat'' in the case where there is no config.
seacat :: PathInfo r
       => (StdMethod -> r -> Handler r) -- ^ Routing function
       -> (String -> Handler r)        -- ^ Top-level error handling function
       -> Migration SqlPersistM       -- ^ Database migration handler
       -> SqlPersistM ()               -- ^ Database populator
       -> IO ()
seacat = seacat'' Nothing

-- |Wrapper for seacat'' in the case where there is config.
seacat' :: PathInfo r
        => ConfigParser                -- ^ Application-specific default configuration (overrides defaults)
        -> (StdMethod -> r -> Handler r) -- ^ Routing function
        -> (String -> Handler r)        -- ^ Top-level error handling function
        -> Migration SqlPersistM       -- ^ Database migration handler
        -> SqlPersistM ()               -- ^ Database populator
        -> IO ()
seacat' cfg = seacat'' $ Just cfg

-- |Fire up the appropriate process, depending on the command.
seacat'' :: PathInfo r
         => Maybe ConfigParser          -- ^ Optional configuration (overrides defaults)
         -> (StdMethod -> r -> Handler r) -- ^ Routing function
         -> (String -> Handler r)        -- ^ Top-level error handling function
         -> Migration SqlPersistM       -- ^ Database migration handler
         -> SqlPersistM ()               -- ^ Database populator
         -> IO ()
seacat'' cfg route on500 migration pop = do
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
    Just conf -> run command route on500 migration pop ((applyUserConfig conf cfg), confFile)
    Nothing   -> die "Failed to read configuration"

-- |Die with a fatal error
die :: String -- ^ The error description
    -> IO ()
die err = putStrLn err >> exitFailure

-------------------------

-- |Function which runs an IO command (eg, runserver)
type CommandRunner = (ConfigParser, Maybe FilePath) -> IO ()

-- |Run one of the applications, depending on the command
run :: PathInfo r
    => String                      -- ^ Command
    -> (StdMethod -> r -> Handler r) -- ^ Routing function
    -> (String -> Handler r)        -- ^ Top-level error handling function
    -> Migration SqlPersistM       -- ^ Database migration handler
    -> SqlPersistM ()               -- ^ Database populator
    -> CommandRunner
run "runserver" = runserver
run "migrate"   = migrate
run "populate"  = populate
run _           = badcommand

-- |Run the server
runserver :: PathInfo r
          => (StdMethod -> r -> Handler r) -- ^ Routing function
          -> (String -> Handler r)        -- ^ Top-level error handling function
          -> a -> b -> CommandRunner
runserver route on500 _ _ c@(conf,_) = do
  let host = get' conf "server" "host"
  let port = get' conf "server" "port"

  let backend  = get' conf "database" "backend"
  let connstr  = get' conf "database" "connection_string" 
  let poolsize = get' conf "database" "pool_size"

  let settings = setHost (fromString host) . setPort port $ defaultSettings

  putStrLn $ "Starting Seacat on " ++ host ++ ":" ++ show port
  withPool (fromString backend) (fromString connstr) poolsize $
    runSettings settings . runner route on500 c

-- |Migrate the database
migrate :: a -> b
        -> Migration SqlPersistM -- ^ Database migration handler
        -> c -> CommandRunner
migrate _ _ migration _ (conf,_) = do
  let backend  = get' conf "database" "backend"
  let connstr  = get' conf "database" "connection_string"
  let poolsize = get' conf "database" "pool_size"
  withDB (fromString backend) (fromString connstr) poolsize $ runMigration migrateAll
  withDB (fromString backend) (fromString connstr) poolsize $ runMigration migration

-- |Populate the database with test data
populate :: a -> b -> c
         -> SqlPersistM () -- ^ Database populator
         -> CommandRunner
populate _ _ _ pop (conf,_) = do
  let backend  = get' conf "database" "backend"
  let connstr  = get' conf "database" "connection_string"
  let poolsize = get' conf "database" "pool_size"
  withDB (fromString backend) (fromString connstr) poolsize pop

-- |Fail with an error
badcommand :: a -> b -> c -> d -> CommandRunner
badcommand _ _ _ _ _ = die "Unknown command"

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
