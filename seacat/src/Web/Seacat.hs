module Web.Seacat (runner) where

import Control.Monad (when)
import Control.Monad.Trans.Reader (runReaderT)
import Data.String (fromString)
import Database.Persist.Sql (ConnectionPool, Migration, SqlPersistM, runMigration)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO.Error (catchIOError)
import Web.Routes.PathInfo (PathInfo)
import Web.Routes.Wai (handleWai)

import Web.Seacat.Configuration (ConfigParser, applyUserConfig, loadConfigFile, defaults, get')
import Web.Seacat.Database (runPool, withPool, withDB)
import Web.Seacat.Types (CommandRunner, Handler, MkUrl)

-- |Fire up the appropriate process, depending on the command.
runner :: PathInfo r
       => Maybe ConfigParser    -- ^ Optional configuration (overrides defaults)
       -> (r -> Handler r)       -- ^ Routing function
       -> (String -> Handler r)  -- ^ Top-level error handling function
       -> Migration SqlPersistM -- ^ Database migration handler
       -> SqlPersistM ()         -- ^ Database populator
       -> IO ()
runner cfg route on500 migration pop = do
  args <- getArgs

  when (length args < 1) $
    die "Expected at least one argument"

  let command = head args

  config <- case args of
             (_:conffile:_) -> loadConfigFile conffile
             _ -> return $ Just defaults

  case config of
    Just conf -> run command route on500 migration pop (applyUserConfig conf cfg)
    Nothing   -> die "Failed to read configuration"

-- |Die with a fatal error
die :: String -- ^ The error description
    -> IO ()
die err = putStrLn err >> exitFailure

-------------------------

-- |Run one of the applications, depending on the command
run :: PathInfo r
    => String                -- ^ Command
    -> (r -> Handler r)       -- ^ Routing function
    -> (String -> Handler r)  -- ^ Top-level error handling function
    -> Migration SqlPersistM -- ^ Database migration handler
    -> SqlPersistM ()         -- ^ Database populator
    -> CommandRunner
run "runserver" = runserver
run "migrate"   = migrate
run "populate"  = populate
run _           = badcommand

-- |Run the server
runserver :: PathInfo r
          => (r -> Handler r)      -- ^ Routing function
          -> (String -> Handler r) -- ^ Top-level error handling function
          -> a -> b -> CommandRunner
runserver route on500 _ _ conf = do
  let host = get' conf "server" "host"
  let port = get' conf "server" "port"

  let connstr  = get' conf "database" "connection_string"
  let poolsize = get' conf "database" "pool_size"

  let settings = setHost (fromString host) . setPort port $ defaultSettings

  putStrLn $ "Starting Seacat on " ++ host ++ ":" ++ show port
  withPool (fromString connstr) poolsize $
    runSettings settings . seacat route on500 conf

-- |Migrate the database
migrate :: a -> b
        -> Migration SqlPersistM -- ^ Database migration handler
        -> c -> CommandRunner
migrate _ _ migration _ conf = do
  let connstr  = get' conf "database" "connection_string"
  let poolsize = get' conf "database" "pool_size"
  withDB (fromString connstr) poolsize $ runMigration migration

-- |Populate the database with test data
populate :: a -> b -> c
         -> SqlPersistM () -- ^ Database populator
         -> CommandRunner
populate _ _ _ pop conf = do
  let connstr  = get' conf "database" "connection_string"
  let poolsize = get' conf "database" "pool_size"
  withDB (fromString connstr) poolsize pop

-- |Fail with an error
badcommand :: a -> b -> c -> d -> CommandRunner
badcommand _ _ _ _ _ = die "Unknown command"

-------------------------

-- |seacat is the actual WAI application. It takes a request, handles
-- it, and produces a response.
seacat :: PathInfo r
       => (r -> Handler r)      -- ^ Routing function
       -> (String -> Handler r) -- ^ Top-level error handling function
       -> ConfigParser         -- ^ The configuration
       -> ConnectionPool       -- ^ Database connection reference
       -> Application
seacat route on500 conf = let webroot = get' conf "server" "web_root"
                          in handleWai (fromString webroot) . process route on500 conf

-- |Route and process a request
-- Todo: use SqlPersistT?
process :: PathInfo r
        => (r -> Handler r)      -- ^ Routing function
        -> (String -> Handler r) -- ^ Top-level error handling function
        -> ConfigParser         -- ^ The configuration
        -> ConnectionPool       -- ^ Database connection reference
        -> MkUrl r              -- ^ URL building function
        -> r                    -- ^ Requested route
        -> Application
process route on500 conf pool mkurl path req = requestHandler `catchIOError` runError
  where requestHandler = runHandler $ route path
        runError err   = runHandler $ on500 (show err)
        runHandler h   = runPool (runReaderT h (conf, mkurl, req)) pool
