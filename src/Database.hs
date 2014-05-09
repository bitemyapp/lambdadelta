{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Database where

import Database.Persist
import Database.Persist.TH
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Post
    number  Int
    board   BoardId

    -- If this is Nothing, this post is the start of a new thread.
    thread  Int Maybe

    -- This is normally the created time, but for a thread it is the time it
    -- was last bumped.
    updated UTCTime default=CURRENT_TIME

    name     String
    email    String
    subject  String
    comment  String
    file     String Maybe
    spoiler  Bool
    password String

    UniquePostID number board

Board
    name     String
    title    String
    subtitle String

    UniqueBoardName name
|]
