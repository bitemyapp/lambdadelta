{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Handler.Templates ( TThread(..)
                         , board, index, thread
                         , Handler.Templates.error) where

import Prelude hiding (null)

import Data.Maybe (isNothing)
import Data.Text (Text, pack, null, strip)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Format (formatTime)
import MyDatabase hiding (Board, File, Post)
import Network.HTTP.Types.Status (Status, statusCode, statusMessage)
import Numeric (showGFloat)
import Routes
import System.Locale (defaultTimeLocale)
import Text.Hamlet (HtmlUrl, hamletFile)

import qualified MyDatabase as D

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

-------------------------

-- |Index template
-- Todo: recent posts/images
-- Todo: title
index :: [[D.Board]] -- ^ The board groupings
      -> HtmlUrl Sitemap
index boardgroups = $(hamletFile "templates/html/index.hamlet")

-- |Board index page template
board :: D.Board     -- ^ The board
      -> [[D.Board]] -- ^ The board groupings
      -> Int         -- ^ The current page
      -> Int         -- ^ The number of pages
      -> [TThread]   -- ^ The list of threads
      -> HtmlUrl Sitemap
board board boardgroups currentPage numPages threads = $(hamletFile "templates/html/board.hamlet")

-- |Thread page template
thread :: D.Board     -- ^ The board
       -> [[D.Board]] -- ^ The board groupings
       -> TThread     -- ^ The thread
       -> HtmlUrl Sitemap
thread board boardgroups thread = $(hamletFile "templates/html/thread.hamlet")

-- |Error template
error :: Status -> String -> HtmlUrl Sitemap
error status description = let code    = statusCode status
                               message = decodeUtf8 $ statusMessage status
                           in $(hamletFile "templates/html/error.hamlet")

-------------------------

-- |Footer template
footer :: HtmlUrl Sitemap
footer = $(hamletFile "templates/html/footer.hamlet")

-- |Top navigation bar
topNavigation :: Maybe (D.Board, Int, Int) -> [[D.Board]] -> HtmlUrl Sitemap
topNavigation = navigation Top

-- |Bottom navigation bar
bottomNavigation :: Maybe (D.Board, Int, Int) -> [[D.Board]] -> HtmlUrl Sitemap
bottomNavigation = navigation Bottom

-- |Navigation bar template
navigation :: Position                  -- ^ Position of the navigation bar (top or bottom), this affects the class
           -> Maybe (D.Board, Int, Int) -- ^ The board we're on, current page, and number of pages
           -> [[D.Board]]               -- ^ The board groupings
           -> HtmlUrl Sitemap
navigation pos board boardgroups = let position = case pos of
                                                    Top -> "top" :: Text
                                                    Bottom -> "bottom" :: Text
                                   in $(hamletFile "templates/html/navigation.hamlet")

-- |Thread template
inlineThread :: D.Board -- ^ The board
             -> TThread -- ^ The thread
             -> Bool    -- ^ Whether to show the reply link or now
             -> HtmlUrl Sitemap
inlineThread board (TThread image op posts imageposts replies) replyButton = $(hamletFile "templates/html/inline_thread.hamlet")

-- |New thread form template
threadForm :: D.Board -- ^ The board
           -> HtmlUrl Sitemap
threadForm board = let thread = Nothing
                       target = Routes.PostThread $ boardName board
                   in $(hamletFile "templates/html/post_form.hamlet")

-- |New reply form template
replyForm :: D.Board -- ^ The board
          -> TThread  -- ^ The OP
          -> HtmlUrl Sitemap
replyForm board (TThread _ op _ _ _) = let thread = Just op
                                           target = PostReply (boardName board) $ postNumber op
                                       in $(hamletFile "templates/html/post_form.hamlet")

-------------------------

-- |Turn a filesize (in bytes) into a nice string
niceSize :: D.File -> Text
niceSize image = let (sz, unit) = niceSize' (fromIntegral $ fileSize image) "B"
                 in pack $ showGFloat (Just 2) sz unit

    where niceSize' sz "B"  | sz > 1024 = niceSize' (sz / 1024.0) "kB"
                            | otherwise = (sz, "B")
          niceSize' sz "kB" | sz > 1024 = (sz / 1024.0, "MB")
                            | otherwise = (sz, "kB")

-- |Produce a list of page numbers
pageList :: Int -> [Int]
pageList last = [1..last]

-- |Check if a text value is nonempty
nonEmpty :: Text -> Bool
nonEmpty = not . null . strip