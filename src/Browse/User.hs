{-# LANGUAGE OverloadedStrings #-}

module Browse.User (index, board, thread, postThread, postReply) where

import Data.Text (Text)

import Network.HTTP.Types.Status
import Network.Wai

import Routes

index :: (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> IO Response
index mkurl req = return $ responseLBS ok200 [] "index"

board :: (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> Text -> IO Response
board mkurl req board = return $ responseLBS ok200 [] "board"

thread :: (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> Text -> Int -> IO Response
thread mkurl req board thread = return $ responseLBS ok200 [] "thread"

postThread :: (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> Text -> IO Response
postThread mkurl req board = return $ responseLBS ok200 [] "post thread"

postReply :: (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> Text -> Int -> IO Response
postReply mkurl req board thread = return $ responseLBS ok200 [] "post reply"
