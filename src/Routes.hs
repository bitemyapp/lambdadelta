{-# LANGUAGE OverloadedStrings #-}

module Routes (Sitemap(..), routeRequest) where

import Control.Applicative

import Data.Text (Text, unpack, pack)

import Network.HTTP.Types.Status
import Network.Wai

import Text.Read (readMaybe)

import Web.Routes.PathInfo

-- |The possible routes on the imageboard
data Sitemap = Index              -- ^ site index: /
             | Board Text         -- ^ board index: /b/
             | Thread Text Int    -- ^ thread display: /b/123
             | PostThread Text    -- ^ new thread: /post/b/
             | PostReply Text Int -- ^ new reply: /post/b/123
               deriving Show

instance PathInfo Sitemap where
    toPathSegments Index = []
    toPathSegments (Board board) = [board]
    toPathSegments (Thread board thread) = [board, pack $ show thread]
    toPathSegments (PostThread board) = ["post", board]
    toPathSegments (PostReply board thread) = ["post", board, pack $ show thread]

    fromPathSegments = patternParse parse

        where parse []             = Right Index
              parse ["post", b]    = Right $ PostThread b
              parse ["post", b, t] = case readMaybe $ unpack t of
                                       Just t' -> Right $ PostReply b t'
                                       Nothing -> fail
              parse [b]            = Right $ Board b
              parse [b, t]         = case readMaybe $ unpack t of
                                       Just t' -> Right $ Thread b t'
                                       Nothing -> fail
              parse _              = fail

              fail = Left "Couldn't parse"

-- |The main router
-- Todo: everything
routeRequest :: (Sitemap -> [(Text, Maybe Text)] -> Text) -> Request -> Sitemap -> Response
routeRequest mkurl req Index = responseLBS ok200 [] "index"
routeRequest mkurl req (Board name) = responseLBS ok200 [] "board"
routeRequest mkurl req (Thread board thread) = responseLBS ok200 [] "thread"
routeRequest mkurl req (PostThread board) = responseLBS ok200 [] "post thread"
routeRequest mkurl req (PostReply board thread) = responseLBS ok200 [] "post reply"
