{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Browse.Templates (board, navigation, footer) where

import Data.Text (Text, pack)

import Database hiding (Board, File, Post)
import qualified Database as D

import Routes

import Text.Hamlet (HtmlUrl, hamletFile)

footer :: HtmlUrl Sitemap
footer = $(hamletFile "templates/html/footer.hamlet")

navigation :: Text -> Maybe (D.Board, Int, Int) -> [[D.Board]] -> HtmlUrl Sitemap
navigation position board boardgroups = $(hamletFile "templates/html/navigation.hamlet")

board :: D.Board -> [[D.Board]] -> Int -> Int -> [((D.File, D.Post), Maybe (Int, Int), [(Maybe D.File, D.Post)])] -> HtmlUrl Sitemap
board board boardgroups currentPage lastPage threads = $(hamletFile "templates/html/board.hamlet")

-- |Turn a filesize (in bytes) into a nice string
niceSize :: D.File -> Text
niceSize image = let (sz, unit)= niceSize' (fromIntegral $ fileSize image) "B"
                 in pack $ show sz ++ unit

    where niceSize' sz "B"  | sz > 1024 = niceSize' (sz / 1024.0) "kB"
                            | otherwise = (sz, "B")
          niceSize' sz "kB" | sz > 1024 = (sz / 1024.0, "MB")
                            | otherwise = (sz, "kB")

-- |Produce a list of page numbers
pageList :: Int -> [Int]
pageList last = [1..last]
