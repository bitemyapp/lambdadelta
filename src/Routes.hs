{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Control.Applicative

import Data.Text (Text, unpack, pack)

import Network.Wai

import Text.Read (readMaybe)

import Web.Routes.PathInfo

-- |The possible routes on the imageboard
data Sitemap = Index              -- ^ site index: /
             | Board Text (Maybe Int) -- ^ board index: /b/<page>
             | Thread Text Int    -- ^ thread display: /b/res/123
             | PostThread Text    -- ^ new thread: /post/b/
             | PostReply Text Int -- ^ new reply: /post/b/123
               deriving Show

instance PathInfo Sitemap where
    toPathSegments Index = []
    toPathSegments (Board board Nothing) = [board]
    toPathSegments (Board board (Just page)) = [board, pack $ show page]
    toPathSegments (Thread board thread) = [board, pack $ show thread]
    toPathSegments (PostThread board) = ["post", board]
    toPathSegments (PostReply board thread) = ["post", board, pack $ show thread]

    fromPathSegments = patternParse parse

        where parse []             = Right Index
              parse ["post", b]    = Right $ PostThread b
              parse ["post", b, t] = case readMaybe $ unpack t of
                                       Just t' -> Right $ PostReply b t'
                                       Nothing -> fail
              parse [b]            = Right $ Board b Nothing
              parse [b, p]         = case readMaybe $ unpack p of
                                       Just p' -> Right $ Board b (Just p')
                                       Nothing -> fail
              parse [b, "res", t]  = case readMaybe $ unpack t of
                                       Just t' -> Right $ Thread b t'
                                       Nothing -> fail
              parse _              = fail

              fail = Left "Couldn't parse"

