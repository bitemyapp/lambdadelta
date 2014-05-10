{-# LANGUAGE OverloadedStrings #-}

module Browse.User (index, board, thread, postThread, postReply) where

import Data.Text (Text)

import Network.HTTP.Types.Status
import Network.Wai

import Routes

index :: (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> Response
index mkurl req = responseLBS ok200 [] "index"

board :: (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> Text -> Response
board mkurl req board = responseLBS ok200 [] "board"

thread :: (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> Text -> Int -> Response
thread mkurl req board thread = responseLBS ok200 [] "thread"

postThread :: (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> Text -> Response
postThread mkurl req board = responseLBS ok200 [] "post thread"

postReply :: (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> Text -> Int -> Response
postReply mkurl req board thread = responseLBS ok200 [] "post reply"
