{-# LANGUAGE FlexibleContexts #-}

module Database where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import Database.Persist hiding (runPool)
import Database.Persist.Sql (ConnectionPool, SqlPersistM, runSqlPersistMPool)
import Database.Persist.Sqlite (withSqlitePool)

-- |Run a database function
withDB :: (MonadIO m, MonadBaseControl IO m)
       => Text          -- ^ The connection string
       -> Int           -- ^ The pool size
       -> SqlPersistM a -- ^ The database function
       -> m a
withDB connstr psize f = withPool connstr psize $ liftIO . runPool f

-- |Run a database function which takes a connection pool
withPool :: (MonadIO m, MonadBaseControl IO m)
         => Text                   -- ^ The connection string
         -> Int                    -- ^ The pool size
         -> (ConnectionPool -> m a) -- ^ The function
         -> m a
withPool = withSqlitePool

-- |Process a database function which takes a connection pool
runPool :: SqlPersistM a -> ConnectionPool -> IO a
runPool = runSqlPersistMPool

-- |Turn a database entity into a value
unentity :: Entity a -> a
unentity (Entity _ val) = val