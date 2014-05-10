{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Browse.Templates (TThread, board) where

import Data.Text (Text, pack)

import Database hiding (Board, File, Post)
import qualified Database as D

import Routes

import Text.Hamlet (HtmlUrl, hamletFile)

-- |Navigation bar position type
-- This isn't exported as the navigation template is only included by other
-- templates
data Position = Top | Bottom

-- |Thread type
data TThread = TThread D.File  -- ^ The OP file
                       D.Post  -- ^ The OP post
                       Int     -- ^ The number of replies
                       Int     -- ^ The number of image replies
                       [(Maybe D.File, D.Post)] -- ^ The replies

-- |Footer template
footer :: HtmlUrl Sitemap
footer = $(hamletFile "templates/html/footer.hamlet")

-- |Navigation bar template
navigation :: Position                  -- ^ Position of the navigation bar (top or bottom), this affects the class
           -> Maybe (D.Board, Int, Int) -- ^ The board we're on, current page, and number of pages
           -> [[D.Board]]               -- ^ The board groupings
           -> HtmlUrl Sitemap
navigation pos board boardgroups = let position = case pos of
                                                    Top -> "top" :: Text
                                                    Bottom -> "bottom" :: Text
                                   in $(hamletFile "templates/html/navigation.hamlet")

-- |Board index page template
board :: D.Board     -- ^ The board
      -> [[D.Board]] -- ^ The board groupings
      -> Int         -- ^ The current page
      -> Int         -- ^ The number of pages
      -> [TThread]   -- ^ The list of threads
      -> HtmlUrl Sitemap
board board boardgroups currentPage numPages threads = $(hamletFile "templates/html/board.hamlet")

-- |Turn a filesize (in bytes) into a nice string
niceSize :: D.File -> Text
niceSize image = let (sz, unit) = niceSize' (fromIntegral $ fileSize image) "B"
                 in pack $ show sz ++ unit

    where niceSize' sz "B"  | sz > 1024 = niceSize' (sz / 1024.0) "kB"
                            | otherwise = (sz, "B")
          niceSize' sz "kB" | sz > 1024 = (sz / 1024.0, "MB")
                            | otherwise = (sz, "kB")

-- |Produce a list of page numbers
pageList :: Int -> [Int]
pageList last = [1..last]
