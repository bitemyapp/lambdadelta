{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Web.Seacat.Database.Internal where

import Database.Persist.TH
import Data.Text (Text)
import Data.Time (UTCTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
IPBan
    -- Where the limit applies. Nothing applies everywhere. (Just x)
    -- applies to all routes tagged x.
    applies String Maybe
    expires UTCTime
    -- eg: "Automatic rate limiting ban"
    reason  Text
    -- Todo: write a fielddef instance for SockAddr, rather than just
    -- `show`ing it
    target  String

    deriving Show
|]
