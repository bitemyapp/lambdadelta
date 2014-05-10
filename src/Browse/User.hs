{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Browse.User (index, board, thread, postThread, postReply) where

import Data.Text (Text)
import Network.HTTP.Types.Status
import Network.Wai
import Routes
import Types

import qualified Browse.Templates as T

-- Todo: can the mkurl and req parameters be hidden from the user?
-- Perhaps in State or something, and then Handlers will be nice and
-- simple to write.

index :: Handler
index = return $ responseLBS ok200 [] "index"

board :: Text -> Maybe Int -> Handler
board board page = return $ responseLBS ok200 [] "board"

thread :: Text -> Int -> Handler
thread board thread = return $ responseLBS ok200 [] "thread"

postThread :: Text -> Handler
postThread board = return $ responseLBS ok200 [] "post thread"

postReply :: Text -> Int -> Handler
postReply board thread = return $ responseLBS ok200 [] "post reply"
