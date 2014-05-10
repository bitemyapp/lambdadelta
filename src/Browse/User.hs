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
index mkurl req = return $ responseLBS ok200 [] "index"

board :: Text -> Maybe Int -> Handler
board board page mkurl req = return $ responseLBS ok200 [] "board"

thread :: Text -> Int -> Handler
thread board thread mkurl req = return $ responseLBS ok200 [] "thread"

postThread :: Text -> Handler
postThread board mkurl req = return $ responseLBS ok200 [] "post thread"

postReply :: Text -> Int -> Handler
postReply board thread mkurl req = return $ responseLBS ok200 [] "post reply"
