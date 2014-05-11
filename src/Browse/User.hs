{-# LANGUAGE OverloadedStrings, Rank2Types, TypeFamilies #-}

module Browse.User (index, board, thread, postThread, postReply) where

import Control.Monad.Trans.Reader (asks)
import Database.Persist
import Data.Text (Text)
import Network.HTTP.Types.Status
import Network.Wai
import Routes
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Types

import qualified Browse.Templates as T
import qualified Database as D

-- |Render the index page
-- Todo: Recent images/posts
-- Todo: Figure out how to nicely handle the html -> response bit
-- Todo: Figure out what the second argument of mkurl is
index :: Handler
index = do boards <- selectList ([] :: [Filter D.Board]) []
           let html = T.index [map (\(Entity _ val) -> val) boards]
           mkurl <- askMkUrl
           let builder = renderHtmlBuilder $ html $ \a -> \b -> mkurl a []
           return $ responseBuilder ok200 [] builder

board :: Text -> Maybe Int -> Handler
board board page = return $ responseLBS ok200 [] "board"

thread :: Text -> Int -> Handler
thread board thread = return $ responseLBS ok200 [] "thread"

postThread :: Text -> Handler
postThread board = return $ responseLBS ok200 [] "post thread"

postReply :: Text -> Int -> Handler
postReply board thread = return $ responseLBS ok200 [] "post reply"
