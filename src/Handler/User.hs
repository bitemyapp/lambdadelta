{-# LANGUAGE OverloadedStrings, Rank2Types, TypeFamilies #-}

module Handler.User (index, board, thread, Handler.User.postThread, postReply) where

import Configuration (conf')
import Control.Monad.Trans.Error (runErrorT)
import Handler
import Handler.Error (error400, error404)
import Handler.Post (newThread, newReply)
import Data.Maybe (catMaybes, fromJust, isJust)
import Database
import MyDatabase
import Database.Persist
import Data.Text (Text)
import Routes (Sitemap)
import Types

import qualified Handler.Templates as T
import qualified MyDatabase as D
import qualified Routes as R

-- |Render the index page
-- Todo: Recent images/posts
index :: Handler Sitemap
index = do boardlisting <- getBoardListing
           html200Response $ T.index boardlisting

-- |Render a board index page
-- Todo: Board-specific config
board :: Text -> Int -> Handler Sitemap
board board page = do summary_size     <- conf' "board" "summary_size"
                      threads_per_page <- conf' "board" "threads_per_page"

                      boardlisting <- getBoardListing
                      maybeBoard <- getBy $ UniqueBoardName board

                      case maybeBoard of
                        Nothing -> error404 "No such board"
                        Just (Entity boardid board) ->
                            do threads <- selectList [PostThread ==. Nothing]
                                                    [ Desc PostUpdated
                                                    , LimitTo threads_per_page
                                                    , OffsetBy $ (page - 1) * threads_per_page]
                               pages <- getNumPages boardid
                               threads' <- mapM (getThread summary_size) threads
                               html200Response $ T.board board boardlisting page pages threads'

thread :: Text -> Int -> Handler Sitemap
thread board thread = do boardlisting <- getBoardListing
                         maybeBoard <- getBy $ UniqueBoardName board

                         case maybeBoard of
                           Nothing -> error404 "No such board"
                           Just (Entity boardid board) -> do
                             maybeThread <- selectFirst [ PostBoard  ==. boardid
                                                       , PostThread ==. Nothing
                                                       , PostNumber ==. thread]
                                                       [ Asc PostTime]
                             case maybeThread of
                               Nothing -> error404 "No such thread"
                               Just post -> do
                                 thread <- getThread (-1) post
                                 html200Response $ T.thread board boardlisting thread

-- |Handle a request to post a new thread
-- Todo: anti-spam
-- Todo: Only respond to POST
-- Todo: Handle noko
postThread :: Text -> Handler Sitemap
postThread board = do
  maybeBoard <- getBy $ UniqueBoardName board

  case maybeBoard of
    Just (Entity boardId _) -> do
      result <- runErrorT $ newThread boardId
      case result of
        Right _ -> redirect $ R.Board board 1
        Left err -> error400 err

    Nothing -> error404 "No such board"

-- | Handle a request to post a new reply
-- Todo: see todos for postThread
postReply :: Text -> Int -> Handler Sitemap
postReply board thread = do
  maybeBoard  <- getBy $ UniqueBoardName board
  case maybeBoard of
    Just (Entity boardId _) -> do
      maybeThread <- getBy $ UniquePostID thread boardId
      case maybeThread of
        Just (Entity threadId _) -> do
          result <- runErrorT $ newReply boardId threadId
          case result of
            Right _ -> redirect $ R.Board board 1
            Left err -> error400 err
        Nothing -> error404 "No such thread"
    Nothing -> error404 "No such board"

-------------------------

-- |Generate the board listing
getBoardListing :: RequestProcessor Sitemap [[D.Board]]
getBoardListing = do board_listing <- conf' "board" "board_listing"
                     listing <- mapM getBoardList board_listing
                     return $ filter ((0/=) . length) listing

    where getBoardList :: [Text] -> RequestProcessor Sitemap [D.Board]
          getBoardList boards = do listing <- mapM (getBy . UniqueBoardName) boards
                                   return $ map unentity $ catMaybes listing

-- |Get the number of pages a board has
getNumPages :: BoardId -- ^ The board
            -> RequestProcessor Sitemap Int
getNumPages board = do threads_per_page <- conf' "board" "threads_per_page"
                       threads <- selectList [ PostBoard ==. board
                                            , PostThread ==. Nothing] []

                       case length threads `quotRem` threads_per_page of
                         (0, _) -> return 1
                         (q, r) | r /= 0 -> return $ q + 1
                         (q, _) -> return q

-- |Get the thread for an OP
getThread :: Int         -- ^ The number of recent posts to show
          -> Entity Post -- ^ The OP, as a unique database entity
          -> RequestProcessor Sitemap T.TThread
getThread limit (Entity opid op) =
    do opFile <- fmap fromJust $ get . fromJust $ postFile op

       replies <- fmap length $ selectList [PostThread ==. Just opid] []

       imageReplies <- fmap length $ selectList [ PostThread ==. Just opid
                                               , PostFile   !=. Nothing
                                               ] []

       posts <- if limit < 0
               then fmap reverse $
                    selectList [PostThread ==. Just opid] [Desc PostUpdated]
               else fmap reverse $
                    selectList [PostThread ==. Just opid]
                               [ Desc PostUpdated
                               , LimitTo limit]

       posts' <- mapM getPostImage posts

       let omittedReplies = replies - length posts'
       let omittedImages  = imageReplies - length (filter (isJust . fst) posts')

       return $ T.TThread opFile op omittedReplies omittedImages posts'


-- |Get the image for a post
getPostImage :: Entity Post -- ^ The post
             -> RequestProcessor Sitemap (Maybe D.File, Post)
getPostImage (Entity _ post) = case postFile post of
                                 Nothing -> return (Nothing, post)
                                 Just fileid -> do file <- get fileid
                                                   return (file, post)
