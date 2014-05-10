{-# LANGUAGE OverloadedStrings #-}

module Browse.User (index, board, thread, postThread, postReply) where

import qualified Browse.Templates as T

import Control.Monad.IO.Class

import Data.Text (Text)

import Network.HTTP.Types.Status
import Network.Wai

import Routes

index :: MonadIO m => (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> m Response
index mkurl req = return $ responseLBS ok200 [] "index"

board :: MonadIO m => (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> Text -> Maybe Int -> m Response
board mkurl req board page = return $ responseLBS ok200 [] "board"

thread :: MonadIO m => (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> Text -> Int -> m Response
thread mkurl req board thread = return $ responseLBS ok200 [] "thread"

postThread :: MonadIO m => (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> Text -> m Response
postThread mkurl req board = return $ responseLBS ok200 [] "post thread"

postReply :: MonadIO m => (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> Text -> Int -> m Response
postReply mkurl req board thread = return $ responseLBS ok200 [] "post reply"
