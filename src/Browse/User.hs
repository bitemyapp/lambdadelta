{-# LANGUAGE OverloadedStrings, Rank2Types, TypeFamilies #-}

module Browse.User (index, board, thread, Browse.User.postThread, postReply) where

import Browse
import Browse.Error (error404)
import Configuration (conf')
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Database
import Database.Persist
import Data.Text (Text)
import Types

import qualified Browse.Templates as T
import qualified Database as D

-- |Render the index page
-- Todo: Recent images/posts
-- Todo: Handle board groups properly
index :: Handler
index = do boards <- fmap (fmap unentity) $ selectList ([] :: [Filter D.Board]) []
           html200Response $ T.index [boards]

-- |Render a board index page
-- Todo: Handle board groups properly
-- Todo: Board-specific config
board :: Text -> Int -> Handler
board board page = do summary_size     <- conf' "board" "summary_size"
                      threads_per_page <- conf' "board" "threads_per_page"

                      boards <- fmap (fmap unentity) $ selectList ([] :: [Filter D.Board]) []
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
                               html200Response $ T.board board [boards] page pages threads'

thread :: Text -> Int -> Handler
thread board thread = utf8200Response "thread"

postThread :: Text -> Handler
postThread board = utf8200Response "post thread"

postReply :: Text -> Int -> Handler
postReply board thread = utf8200Response "post reply"

-------------------------

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

       posts <- fmap reverse $
               selectList [PostThread ==. Just opid]
                          [ Desc PostUpdated
                          , LimitTo limit
                          ]
       posts' <- (mapM getPostImage posts) :: RequestProcessor [(Maybe File, Post)]

       let posts'' = posts' :: [(Maybe File, Post)]

       let omittedReplies = replies - length posts'
       let omittedImages  = imageReplies - length (filter (not . isNothing . fst) posts')

       return $ T.TThread opFile op omittedReplies omittedImages posts'


-- |Get the image for a post
getPostImage :: Entity Post -- ^ The post
             -> RequestProcessor (Maybe File, Post)
getPostImage (Entity postid post) = case postFile post of
                                      Nothing -> return (Nothing, post)
                                      Just fileid -> do file <- get fileid
                                                        return (file, post)
