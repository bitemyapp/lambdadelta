{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Handler.Post ( newThread
                    , newReply) where

-- Todo: Replace calls to isJust/fromJust with pattern matching

import Prelude hiding (concat, null)

import Control.Applicative ((<$>))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Error (ErrorT, throwError)
import Data.ByteString (concat)
import Data.Char (chr)
import Data.Hash.MD5 (Str(..), md5s)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Text (Text, null, pack, strip, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Database
import Database.Persist
import Graphics.ImageMagick.MagickWand
import Handler.Admin (bump, deleteThread)
import Network.Wai.Parse (FileInfo(..), Param, lbsBackEnd, parseRequestBody)
import Routes (Sitemap)
import System.FilePath.Posix (joinPath, takeExtension)
import Web.Seacat.Configuration (conf')
import Web.Seacat.Types (RequestProcessor, askReq)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wai.Parse as W

-- |Post a new thread, returning the file and post IDs on success.
-- Todo: Take the board name, and produce a nice error when it doesn't
-- exist.
newThread :: BoardId -- ^ The board
          -> ErrorT String (RequestProcessor Sitemap) (FileId, PostId)
newThread board = do
  (Just f, p) <- handlePostForm board Nothing

  -- Purge any threads which fell off the back page
  threads_per_page <- lift $ conf' "board" "threads_per_page"
  maximum_pages <- lift $ conf' "board" "maximum_pages"
  lastThread <- lift $ selectFirst [ PostThread ==. Nothing
                                  , PostBoard ==. board]
                                  [ OffsetBy $ (maximum_pages - 1) * threads_per_page
                                  , LimitTo 1]

  -- If this is a Nothing, the board isn't full yet, so we don't need
  -- to do anything.
  lift $ case lastThread of
    Just (Entity _ thread) -> do
      threads <- selectList [ PostThread ==. Nothing
                           , PostBoard ==. board
                           , PostUpdated <. postUpdated thread] []
      mapM_ deleteThread threads
    Nothing -> return ()

  return (f, p)

-- |Post a new reply, returning the file (if it exists) and post IDs
-- on success.
-- Todo: Take the board name and thread number, and produce nice
-- errors when they don't exist.
newReply :: BoardId -- ^ The board
         -> PostId  -- ^ The OP
         -> ErrorT String (RequestProcessor Sitemap) (Maybe FileId, PostId)
newReply board = handlePostForm board . Just

-------------------------

-- |Handle a post form, saving the post (and optional file) into the
-- database, returning the IDs. This returns Nothing if something went
-- wrong.
-- Todo: Use digestive-functors
-- Todo: Store temporary files on disk, not in memory
-- Todo: tripcodes - come up with a good way for the name field to
-- modify the post
-- Todo: sage/noko/dice - come up with a good way for the email field
-- to modify the response and post
handlePostForm :: BoardId      -- ^ The board
               -> Maybe PostId -- ^ The OP
               -> ErrorT String (RequestProcessor Sitemap) (Maybe FileId, PostId)
handlePostForm boardId threadId = do
  request <- lift askReq
  (params, files) <- liftIO $ parseRequestBody lbsBackEnd request

  let comment = decodeUtf8 <$> lookup "comment"  params
  let file    = lookup "file" files

  -- If this is a new thread, there must be both an image and a comment.
  -- If this is a reply, there must be at least one of an image or a comment.
  let isValidPost = case threadId of
                      Just _  -> hasValue comment || hasContent file
                      Nothing -> hasValue comment && hasContent file
  
  -- Return an error if the post isn't good
  unless isValidPost $ case threadId of
                         Just _  -> replyError
                         Nothing -> threadError comment file

  -- All looks good, construct a post, save the file, and bump the thread.
  lift $ commitPost boardId threadId params files

-- |Commit a new post and possible file upload to the database
commitPost :: BoardId                -- ^ The board
           -> Maybe PostId           -- ^ The OP
           -> [Param]                -- ^ The parameters
           -> [W.File BL.ByteString] -- ^ The files
           -> RequestProcessor Sitemap (Maybe FileId, PostId)
commitPost boardId threadId params files = do
  let name     = decodeUtf8 <$> lookup "name"     params
  let email    = decodeUtf8 <$> lookup "email"    params
  let subject  = decodeUtf8 <$> lookup "subject"  params
  let comment  = decodeUtf8 <$> lookup "comment"  params
  let password = decodeUtf8 <$> lookup "password" params

  let file    = lookup "file" files
  let spoiler = isJust $ lookup "spoiler" params

  board  <- fromJust <$> get boardId
  fileId <- if hasContent file
           then Just <$> handleFileUpload board (fromJust file) spoiler
           else return Nothing
  postId <- handleNewPost boardId threadId name email subject comment fileId password

  -- bump the thread if there is a thread to bump
  return () `maybe` bump $ threadId

  -- return the relevant information
  return (fileId, postId)

-------------------------

-- |Upload a possible file, returning the ID
-- Todo: Implement maximum file sizes
-- Todo: Implement file type restrictions
handleFileUpload :: Board                  -- ^ The board
                 -> FileInfo BL.ByteString -- ^ The file
                 -> Bool                   -- ^ Whether it is spoilered
                 -> RequestProcessor Sitemap FileId
handleFileUpload board (FileInfo fname _ content) spoiler = do
  thumbnail_width  <- conf' "board" "thumbnail_width"
  thumbnail_height <- conf' "board" "thumbnail_height"

  -- Construct the target file path
  fileroot <- conf' "server" "file_root"
  let fname' = map (chr . fromIntegral) $ B.unpack fname
  let fnamehash = md5s (Str fname') ++ takeExtension fname'
  let size = fromIntegral $ BL.length content

  -- Save the file
  let path = joinPath [fileroot, unpack $ boardName board, "src", fnamehash]
  liftIO $ BL.writeFile path content


  -- Get its dimensions
  let img = concat $ BL.toChunks content
  (width, height) <- liftIO . withMagickWandGenesis $ do
    (_, w) <- magickWand
    readImageBlob w img
    width  <- getImageWidth w
    height <- getImageHeight w

    return (width, height)

  -- Produce a thumbnail
  let thumbpath = joinPath [fileroot, unpack $ boardName board, "thumb", fnamehash]
  thumbnail <- liftIO . withMagickWandGenesis $ do
    let w = fromIntegral width :: Float
    let h = fromIntegral height :: Float

    let (width', height') = if w / thumbnail_width > h / thumbnail_height
                            then (floor thumbnail_width, floor $ thumbnail_width * h / w)
                            else (floor $ thumbnail_height * w / h, floor thumbnail_height)

    (_, w) <- magickWand
    readImageBlob w img
    resizeImage w width' height' lanczosFilter 1
    setImageCompressionQuality w 95
    getImageBlob w

  liftIO $ B.writeFile thumbpath thumbnail

  insert $ File (pack fnamehash) (pack fname') size width height spoiler

-- |Construct and insert a new post into the database
handleNewPost :: BoardId      -- ^ The board
              -> Maybe PostId -- ^ The OP (if a reply)
              -> Maybe Text   -- ^ The name
              -> Maybe Text   -- ^ The email
              -> Maybe Text   -- ^ The subject
              -> Maybe Text   -- ^ The comment
              -> Maybe FileId -- ^ The file ID
              -> Maybe Text   -- ^ The password
              -> RequestProcessor Sitemap PostId
handleNewPost boardId threadId name email subject comment fileId password = do
  number  <- ((+1) . length) <$> selectList [PostBoard ==. boardId] []
  updated <- liftIO getCurrentTime

  let name'     = if hasValue name then fromJust name else "Anonymous"
  let email'    = fromMaybe "" email
  let subject'  = fromMaybe "" subject
  let comment'  = fromMaybe "" comment
  let password' = fromMaybe "" password

  insert $ Post number boardId threadId updated updated fileId name' email' subject' comment' password'

-------------------------

-- |Check if a value is set and is nonempty
hasValue :: Maybe Text -> Bool
hasValue Nothing = False
hasValue (Just t) = not $ null (strip t)

-- |Check if a file is nonempty
hasContent :: Maybe (FileInfo BL.ByteString) -> Bool
hasContent Nothing = False
hasContent (Just (FileInfo _ _ c)) = not $ BL.null c

-------------------------

-- |Get the error message for a reply
replyError :: Monad m => ErrorT String m a
replyError = throwError "Replies must have at least a file or a comment"

-- |Get the error message for a thread
threadError :: Monad m
            => Maybe Text -- ^ The comment
            -> Maybe (FileInfo BL.ByteString) -- ^ The file
            -> ErrorT String m a
threadError com fil = throwError $ threadError' (hasValue com) (hasContent fil)
  where threadError' False False = "Topics must have both a file and a comment."
        threadError' True False  = "Topics must have a file."
        threadError' False True  = "Topics must have a comment"