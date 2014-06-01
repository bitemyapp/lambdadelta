module Handler.Admin ( bump
                     , deleteThread
                     , deletePost) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Data.Time.Clock (getCurrentTime)
import Database
import Database.Persist
import Routes (Sitemap)
import System.Directory (removeFile)
import System.FilePath.Posix (joinPath)
import System.IO.Error (catchIOError)
import Web.Seacat
import Web.Seacat.Configuration (conf')

-- |Bump a thread if it's below the bump limit
bump :: PostId -- ^ The OP
     -> RequestProcessor Sitemap ()
bump threadId = do
  bump_limit <- conf' "board" "bump_limit"
  replies <- length <$> selectList [PostThread ==. Just threadId] []

  when (replies < bump_limit) $ do
    now <- liftIO getCurrentTime
    update threadId [PostUpdated =. now]

-------------------------

-- |Delete a thread and all its posts
deleteThread :: Entity Post -- ^ The thread
             -> RequestProcessor Sitemap ()
deleteThread (Entity threadId op) = do
  posts <- selectList [ PostBoard ==. postBoard op
                     , PostThread ==. Just threadId] []

  mapM_ deletePost posts
  deletePost $ Entity threadId op

-- |Delete a post and its associated file (if any)
deletePost :: Entity Post -- ^ The post
           -> RequestProcessor Sitemap ()
deletePost (Entity postId post) = do
  board <- fromJust <$> get (postBoard post)

  -- Delete the file and thumbnail
  case postFile post of
    Just fid -> do
      file <- get fid
      case file of
        Just f -> do
          fileroot <- conf' "server" "file_root"

          let fname = joinPath [fileroot, unpack $ boardName board, "src", unpack $ Database.fileName f]
          let thumb = joinPath [fileroot, unpack $ boardName board, "thumb", unpack $ Database.fileName f]

          liftIO $ removeFile fname `catchIOError` (\_ -> return ())
          liftIO $ removeFile thumb `catchIOError` (\_ -> return ())
        _ -> return ()
    _ -> return ()

  -- Delete the post
  delete postId
  return ()
