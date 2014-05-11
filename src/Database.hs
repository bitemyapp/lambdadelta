{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Database where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Database.Persist.TH
import Data.Time
import Data.Text (Text)
import Database.Persist.Sql (ConnectionPool, SqlPersistM, runSqlPersistMPool)
import Database.Persist.Sqlite (withSqlitePool)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Post
    number  Int
    board   BoardId

    -- If this is Nothing, this post is the start of a new thread.
    thread  Int Maybe

    -- This is normally the created time, but for a thread it is the time it
    -- was last bumped.
    updated UTCTime default=CURRENT_TIME

    file FileId Maybe

    name     Text
    email    Text
    subject  Text
    comment  Text
    password Text

    UniquePostID number board

Board
    name     Text
    title    Text
    subtitle Text

    UniqueBoardName name

File
    name     Text
    origname Text
    post     PostId

    -- In bytes
    size     Int

    width    Int
    height   Int
    spoiler  Bool
|]

-------------------------

-- |Run a database function
withDB :: (MonadIO m, MonadBaseControl IO m)
       => Text          -- ^ The connection string
       -> Int           -- ^ The pool size
       -> SqlPersistM a -- ^ The database function
       -> m a
withDB connstr psize f = withPool connstr psize $ liftIO . runSqlPersistMPool f

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
