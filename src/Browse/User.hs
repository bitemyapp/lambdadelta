{-# LANGUAGE OverloadedStrings, Rank2Types, TypeFamilies #-}

module Browse.User (index, board, thread, Browse.User.postThread, postReply) where

import Browse
import Browse.Error (error404)
import Configuration (conf')
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing)
import Database
import Database.Persist
import Data.Text (Text)
import Types

import qualified Browse.Templates as T
import qualified Database as D

-- |Render the index page
-- Todo: Recent images/posts
index :: Handler
index = do boardlisting <- getBoardListing
           html200Response $ T.index boardlisting

-- |Render a board index page
-- Todo: Board-specific config
board :: Text -> Int -> Handler
board board page = do summary_size     <- conf' "board" "summary_size"
                      threads_per_page <- conf' "board" "threads_per_page"

                      boardlisting <- getBoardListing
                      maybeBoard <- getBy $ UniqueBoardName board

                      case maybeBoard of
                        Nothing -> error404 "No such board"
                        Just (Entity boardid board) ->
                            do threads <- selectList [PostThread ==. Nothing]
                                                    [ Asc PostUpdated
                                                    , LimitTo threads_per_page
                                                    , OffsetBy $ (page - 1) * threads_per_page]
                               pages <- getNumPages boardid
                               threads' <- mapM (getThread summary_size) threads
                               html200Response $ T.board board boardlisting page pages threads'

thread :: Text -> Int -> Handler
thread board thread = do boardlisting <- getBoardListing
                         maybeBoard <- getBy $ UniqueBoardName board

                         case maybeBoard of
                           Nothing -> error404 "No such board"
                           Just (Entity boardid board) -> do
                             maybeThread <- selectFirst [ PostBoard  ==. boardid
                                                       , PostThread ==. Nothing
                                                       , PostNumber ==. thread] []
                             case maybeThread of
                               Nothing -> error404 "No such thread"
                               Just post -> do
                                 thread <- getThread (-1) post
                                 html200Response $ T.thread board boardlisting thread

postThread :: Text -> Handler
postThread board = utf8200Response "post thread"

postReply :: Text -> Int -> Handler
postReply board thread = utf8200Response "post reply"

-------------------------

-- |Generate the board listing
getBoardListing :: RequestProcessor [[D.Board]]
getBoardListing = do board_listing <- conf' "board" "board_listing"
                     listing <- mapM getBoardList board_listing
                     return $ filter ((0/=) . length) listing

    where getBoardList :: [Text] -> RequestProcessor [D.Board]
          getBoardList boards = do listing <- mapM (getBy . UniqueBoardName) boards
                                   return $ map unentity $ catMaybes listing

-- |Get the number of pages a board has
getNumPages :: BoardId -- ^ The board
            -> RequestProcessor Int
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
          -> RequestProcessor T.TThread
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
       let omittedImages  = imageReplies - length (filter (not . isNothing . fst) posts')

       return $ T.TThread opFile op omittedReplies omittedImages posts'


-- |Get the image for a post
getPostImage :: Entity Post -- ^ The post
             -> RequestProcessor (Maybe File, Post)
getPostImage (Entity _ post) = case postFile post of
                                 Nothing -> return (Nothing, post)
                                 Just fileid -> do file <- get fileid
                                                   return (file, post)
