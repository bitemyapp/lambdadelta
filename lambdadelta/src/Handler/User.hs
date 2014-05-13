{-# LANGUAGE OverloadedStrings, Rank2Types, TypeFamilies #-}

module Handler.User (index, board, thread, Handler.User.postThread, postReply) where

import Control.Applicative ((<$>))
import Control.Monad.Trans.Error (ErrorT)
import Control.Monad.Trans.Error (runErrorT)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Text (Text)
import Database
import Database.Persist
import Handler.Error (error400, error404)
import Handler.Post (newThread, newReply)
import Routes (Sitemap)
import Web.Seacat.Configuration (conf')
import Web.Seacat.Database
import Web.Seacat.RequestHandler
import Web.Seacat.Types

import qualified Handler.Templates as T
import qualified Database as D
import qualified Routes as R

-- |Render the index page
-- Todo: Recent images/posts
index :: Handler Sitemap
index = getBoardListing >>= html200Response . T.index

-- |Render a board index page
-- Todo: Board-specific config
board :: Text -> Int -> Handler Sitemap
board board page = withBoard board $ \(Entity boardId board') -> do
                     summary_size     <- conf' "board" "summary_size"
                     threads_per_page <- conf' "board" "threads_per_page"
                     boardlisting <- getBoardListing

                     threads <- selectList [ PostBoard ==. boardId
                                          , PostThread ==. Nothing]
                                          [ Desc PostUpdated
                                          , LimitTo threads_per_page
                                          , OffsetBy $ (page - 1) * threads_per_page]
                     pages <- getNumPages boardId
                     threads' <- mapM (getThread summary_size) threads
                     html200Response $ T.board board' boardlisting page pages threads'

thread :: Text -> Int -> Handler Sitemap
thread board thread = withBoard board $ \(Entity boardId board') ->
                        withThread boardId thread $ \thread' -> do
                          boardlisting <- getBoardListing
                          thread'' <- getThread (-1) thread'
                          html200Response $ T.thread board' boardlisting thread''

-- |Handle a request to post a new thread
-- Todo: anti-spam
-- Todo: Only respond to POST
-- Todo: Handle noko
postThread :: Text -> Handler Sitemap
postThread board = withBoard board $ \(Entity boardId _) ->
  possiblyRedirect (newThread boardId) $ R.Board board 1

-- | Handle a request to post a new reply
-- Todo: see todos for postThread
postReply :: Text -> Int -> Handler Sitemap
postReply board thread = withBoard board $ \(Entity boardId _) ->
  withThread boardId thread $ \(Entity threadId _) ->
    possiblyRedirect (newReply boardId threadId) $ R.Board board 1

-------------------------

-- |Run a handler which takes a board as a parameter, giving a 404
-- error if the board doesn't exist.
withBoard :: Text -- ^ The board name
          -> (Entity Board -> Handler Sitemap) -- ^ The handler
          -> Handler Sitemap
withBoard board handler = do
  maybeBoard <- getBy $ UniqueBoardName board
  case maybeBoard of
    Just b  -> handler b
    Nothing -> error404 "No such board"

-- |Run a handler which takes a thread as a parameter, giving a 404
-- error if the thread doesn't exist.
withThread :: BoardId -- ^ The board
           -> Int     -- ^ The thread number
           -> (Entity Post -> Handler Sitemap) -- ^ The handler
           -> Handler Sitemap
withThread boardId thread handler = do
  maybeThread <- selectFirst [ PostNumber ==. thread
                            , PostThread ==. Nothing
                            , PostBoard  ==. boardId ] []
  case maybeThread of
    Just t  -> handler t
    Nothing -> error404 "No such thread"

-------------------------

-- |Run a possibly failing handler, and redirect on success.
-- Todo: Add some way of defining the redirect later (ie, noko)
possiblyRedirect :: ErrorT String (RequestProcessor Sitemap) a -- ^ The possibly-failing handler
                 -> Sitemap -- ^ The location to redirect to
                 -> Handler Sitemap
possiblyRedirect failing target = do
  result <- runErrorT failing
  case result of
    Right _  -> redirect target
    Left err -> error400 err

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

       replies <- length <$> selectList [PostThread ==. Just opid] []

       imageReplies <- length <$> selectList [ PostThread ==. Just opid
                                               , PostFile   !=. Nothing
                                               ] []

       posts <- reverse <$> selectList [PostThread ==. Just opid]
                 (if limit < 0
                  then [Desc PostUpdated]
                  else [Desc PostUpdated, LimitTo limit])

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
