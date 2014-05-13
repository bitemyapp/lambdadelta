{-# LANGUAGE OverloadedStrings, Rank2Types, TypeFamilies #-}

module Handler.User (index, board, thread, Handler.User.postThread, postReply) where

import Control.Monad.Trans.Error (ErrorT, runErrorT)
import Data.Text (Text)
import Database
import Database.Persist
import Handler.Board (listing, numPages, getThread)
import Handler.Error (error400, error404)
import Handler.Post (newThread, newReply)
import Routes (Sitemap)
import Web.Seacat.Configuration (conf')
import Web.Seacat.RequestHandler
import Web.Seacat.RequestHandler.Types

import qualified Handler.Templates as T
import qualified Routes as R

-- |Render the index page
-- Todo: Recent images/posts
index :: Handler Sitemap
index = listing >>= html200Response . T.index

-- |Render a board index page
-- Todo: Board-specific config
board :: Text -> Int -> Handler Sitemap
board board page = withBoard board $ \(Entity boardId board') -> do
                     summary_size     <- conf' "board" "summary_size"
                     threads_per_page <- conf' "board" "threads_per_page"
                     boardlisting <- listing

                     threads <- selectList [ PostBoard ==. boardId
                                          , PostThread ==. Nothing]
                                          [ Desc PostUpdated
                                          , LimitTo threads_per_page
                                          , OffsetBy $ (page - 1) * threads_per_page]
                     pages    <- numPages boardId
                     threads' <- mapM (getThread summary_size) threads
                     html200Response $ T.board board' boardlisting page pages threads'

thread :: Text -> Int -> Handler Sitemap
thread board thread = withBoard board $ \(Entity boardId board') ->
                        withThread boardId thread $ \thread' -> do
                          boardlisting <- listing
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
