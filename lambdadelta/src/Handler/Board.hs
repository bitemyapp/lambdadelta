module Handler.Board ( listing
                     , numPages
                     , getThread
                     , getPostImage) where

import Control.Applicative ((<$>))
import Data.Maybe (catMaybes, fromJust, isJust)
import Database
import Database.Persist
import Handler.Templates (TThread(..))
import Routes (Sitemap)
import Web.Seacat.Configuration (conf')
import Web.Seacat.Database
import Web.Seacat.RequestHandler.Types (RequestProcessor)

import qualified Database as D

-- |Generate the board listing
listing :: RequestProcessor Sitemap [[D.Board]]
listing = do
  board_listing <- conf' "board" "board_listing"
  blist <- mapM getBoardList board_listing
  return $ filter ((0/=) . length) blist

  where getBoardList boards = do
          blist <- mapM (getBy . UniqueBoardName) boards
          return $ map unentity $ catMaybes blist

-- |Get the number of pages a board has
numPages :: BoardId -- ^ The board
         -> RequestProcessor Sitemap Int
numPages board = do
  threads_per_page <- conf' "board" "threads_per_page"
  threads <- selectList [ PostBoard ==. board
                       , PostThread ==. Nothing] []

  case length threads `quotRem` threads_per_page of
    (0, _) -> return 1
    (q, r) | r /= 0 -> return $ q + 1
    (q, _) -> return q

-- |Get the thread for an OP
getThread :: Int         -- ^ The number of recent posts to show
          -> Entity Post -- ^ The OP, as a unique database entity
          -> RequestProcessor Sitemap TThread
getThread limit (Entity opid op) = do
  opFile <- fmap fromJust $ get . fromJust $ postFile op

  replies <- length <$> selectList [PostThread ==. Just opid] []

  imageReplies <- length <$> selectList [ PostThread ==. Just opid
                                       , PostFile   !=. Nothing
                                       ] []

  posts <- reverse <$> selectList [PostThread ==. Just opid]
            (if limit < 0
             then [Desc PostUpdated]
             else [Desc PostUpdated, LimitTo limit])

  posts' <- mapM (getPostImage . unentity) posts

  let omittedReplies = replies - length posts'
  let omittedImages  = imageReplies - length (filter (isJust . fst) posts')

  return $ TThread opFile op omittedReplies omittedImages posts'


-- |Get the image for a post
getPostImage :: Post -- ^ The post
             -> RequestProcessor Sitemap (Maybe D.File, Post)
getPostImage post = case postFile post of
                      Just fileid -> get fileid >>= \file -> return (file, post)
                      Nothing -> return (Nothing, post)
