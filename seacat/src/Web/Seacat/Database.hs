{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Web.Seacat.Database where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, SqlPersistM, runSqlPersistMPool)
import Database.Persist.Sqlite (withSqlitePool)
import Database.Persist.TH

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

--------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
IPBan
    -- Where the limit applies. Nothing applies to all checked
    -- routes. (Just x) applies to all routes tagged x.
    applies String Maybe
    expires UTCTime
    reason  Text

    -- These are IP addresses converted to numbers for comparison.
    -- Todo: Use a proper IP range type (requires more instances)
    start Rational
    stop  Rational
    deriving Show

RateLimit
    applies String Maybe
    expires UTCTime
    target  String
    deriving Show
|]
