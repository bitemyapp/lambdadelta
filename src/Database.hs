{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Database where

import Database.Persist
import Database.Persist.TH
import Data.Time
import Data.Text (Text)

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
