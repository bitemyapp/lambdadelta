{-# LANGUAGE OverloadedStrings, Rank2Types, TypeFamilies #-}

module Browse.User (index, board, thread, postThread, postReply) where

import Browse
import Database.Persist
import Data.Text (Text)
import Network.Wai
import Types

import qualified Browse.Templates as T
import qualified Database as D

-- |Render the index page
-- Todo: Recent images/posts
index :: Handler
index = do boards <- selectList ([] :: [Filter D.Board]) []
           html200Response $ T.index [map (\(Entity _ val) -> val) boards]

board :: Text -> Maybe Int -> Handler
board board page = utf8200Response "board"

thread :: Text -> Int -> Handler
thread board thread = utf8200Response "thread"

postThread :: Text -> Handler
postThread board = utf8200Response "post thread"

postReply :: Text -> Int -> Handler
postReply board thread = utf8200Response "post reply"
