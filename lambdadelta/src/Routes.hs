{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Data.Text (Text, unpack, pack)
import Text.Read (readMaybe)
import Web.Routes.PathInfo

-- |The possible routes on the imageboard
-- Todo: Stylesheet takes the name of the stylesheet as a parameter
data Sitemap = Index              -- ^ site index: /
             | Board Text Int     -- ^ board index: /b/<page>
             | Thread Text Int    -- ^ thread display: /b/res/123
             | Post Text Int Int  -- ^ individual post: /b/res/123#5
             | PostThread Text    -- ^ new thread: /post/b/
             | PostReply Text Int -- ^ new reply: /post/b/123
             | File Text Text     -- ^ files: /b/src/123.png
             | Thumb Text Text    -- ^ thumbnails: /b/thumb/123.png
             | Stylesheet         -- ^ stylesheets: /style.css
             | Banner             -- ^ banners: /banner.png
             | Error404           -- ^ catch-all route
               deriving Show

instance PathInfo Sitemap where
    toPathSegments Index = []
    toPathSegments Stylesheet = ["style.css"]
    toPathSegments Banner = ["banner.png"]
    toPathSegments (Board board 1) = [board]
    toPathSegments (Thread board thread) = [board, "res", pack $ show thread]
    toPathSegments (Post board thread post) = [board, "res", pack $ show thread ++ "#" ++ show post]
    toPathSegments (PostThread board) = ["post", board]
    toPathSegments (PostReply board thread) = ["post", board, pack $ show thread]
    toPathSegments (File board file) = [board, "src", file]
    toPathSegments (Thumb board file) = [board, "thumb", file]

    fromPathSegments = patternParse parse

        where parse = Right . parse'

              parse' []             = Index

              parse' ["post", b, ""]    = parse' ["post", b]
              parse' ["post", b, t, ""] = parse' ["post", b, t]
              parse' [b, ""]            = parse' [b]
              parse' [b, p, ""]         = parse' [b, p]
              parse' [b, "res", t, ""]  = parse' [b, "res", t]

              parse' ["style.css"]  = Stylesheet
              parse' ["banner.png"] = Banner
              parse' ["post", b]    = PostThread b
              parse' ["post", b, t] = case readMaybe $ unpack t of
                                       Just t' -> PostReply b t'
                                       Nothing -> Error404
              parse' [b]            = Board b 1
              parse' [b, p]         = case readMaybe $ unpack p of
                                       Just p' -> Board b p'
                                       Nothing -> Error404
              parse' [b, "res", t]  = case readMaybe $ unpack t of
                                       Just t' -> Thread b t'
                                       Nothing -> Error404
              parse' [b, "src", f]   = File b f
              parse' [b, "thumb", f] = Thumb b f

              parse' _              = Error404
