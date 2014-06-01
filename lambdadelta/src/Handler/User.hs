{-# LANGUAGE OverloadedStrings, Rank2Types, TypeFamilies #-}

module Handler.User (index, board, thread, Handler.User.postThread, postReply) where

import Control.Arrow (second)
import Control.Monad.Trans.Error (ErrorT, runErrorT)
import Data.Text (Text)
import Database
import Database.Persist
import Handler.Board (listing, numPages, getThread)
import Handler.Error (error400, error404)
import Handler.Post (Target(..), newThread, newReply)
import Network.HTTP.Types.Status (ok200)
import Routes (Sitemap)
import Text.Hamlet (HtmlUrl)
import Web.Seacat (Handler, RequestProcessor, askMkUrl, conf', htmlResponse, redirect)

import qualified Handler.Templates as T
import qualified Routes as R

-- |Render the index page
-- Todo: Recent images/posts
index :: Handler Sitemap
index = do
  lst <- listing

  mkurl <- askMkUrl
  htmlResponse $ T.index lst $ \a b ->
    mkurl a $ map (second Just) b

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

  mkurl <- askMkUrl
  htmlResponse $ T.board board' boardlisting page pages threads' $ \a b ->
    mkurl a $ map (second Just) b

thread :: Text -> Int -> Handler Sitemap
thread board thread = withBoard board $ \(Entity boardId board') ->
  withThread boardId thread $ \thread' -> do
    boardlisting <- listing
    thread'' <- getThread (-1) thread'

    mkurl <- askMkUrl
    htmlResponse $ T.thread board' boardlisting thread'' $ \a b ->
      mkurl a $ map (second Just) b

-- |Handle a request to post a new thread
-- Todo: anti-spam
postThread :: Text -> Handler Sitemap
postThread board = withBoard board $ \(Entity boardId _) ->
  possiblyRedirect (newThread boardId) $ \(a,_,p) -> if a == Index
                                                    then R.Board board 1
                                                    else R.Thread board p

-- | Handle a request to post a new reply
-- Todo: see todos for postThread
postReply :: Text -> Int -> Handler Sitemap
postReply board thread = withBoard board $ \(Entity boardId _) ->
  withThread boardId thread $ \(Entity threadId _) ->
    possiblyRedirect (newReply boardId threadId) $ \(a,_,p) -> if a == Index
                                                              then R.Board board 1
                                                              else R.Post board thread p

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
possiblyRedirect :: ErrorT String (RequestProcessor Sitemap) a -- ^ The possibly-failing handler
                 -> (a -> Sitemap) -- ^ The location to redirect to
                 -> Handler Sitemap
possiblyRedirect failing target = do
  result <- runErrorT failing
  case result of
    Right res -> redirect $ target res
    Left err  -> error400 err
