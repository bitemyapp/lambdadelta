{-# LANGUAGE FlexibleContexts #-}

module Web.Seacat.Database ( withDB
                           , withPool
                           , runPool
                           , unentity) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.String (IsString, fromString)
import Database.Persist hiding (runPool)
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, SqlPersistM, runSqlPersistMPool)
import Database.Persist.Sqlite (withSqlitePool)

-- |List of database backends
data Backend = Sqlite | Postgres

instance IsString Backend where
    fromString "sqlite"     = Sqlite
    fromString "postgres"   = Postgres
    fromString "postgresql" = Postgres

    -- Using `error` isn't great, but this is a fatal, unrecoverable,
    -- thing.
    fromString str = error $ "Unknown database backend " ++ str

-- |Run a database function
withDB :: (MonadIO m, MonadBaseControl IO m)
       => Backend       -- ^ The backend type
       -> Text          -- ^ The connection string
       -> Int           -- ^ The pool size
       -> SqlPersistM a -- ^ The database function
       -> m a
withDB backend connstr psize f = withPool backend connstr psize $ liftIO . runPool f

-- |Run a database function which takes a connection pool
withPool :: (MonadIO m, MonadBaseControl IO m)
         => Backend                -- ^ The backend type
         -> Text                   -- ^ The connection string
         -> Int                    -- ^ The pool size
         -> (ConnectionPool -> m a) -- ^ The function
         -> m a
withPool Sqlite   = withSqlitePool
withPool Postgres = withPostgresqlPool . encodeUtf8

-- |Process a database function which takes a connection pool
runPool :: SqlPersistM a -> ConnectionPool -> IO a
runPool = runSqlPersistMPool

-- |Turn a database entity into a value
unentity :: Entity a -> a
unentity (Entity _ val) = val