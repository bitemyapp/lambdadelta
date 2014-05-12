{-# LANGUAGE OverloadedStrings #-}

module Handler.Post ( newThread
                    , newReply) where

-- Todo: Replace calls to isJust/fromJust with pattern matching
-- Todo: Use the error monad all over the place

import Prelude hiding (concat, null)

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Configuration (conf')
import Data.ByteString (concat)
import Data.Char (chr)
import Data.Text (Text, null, pack, strip, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Hash.MD5 (Str(..), md5s)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Database
import Database.Persist ((==.), (=.),  get, insert, selectList, update)
import Graphics.ImageMagick.MagickWand (getImageHeight, getImageWidth, magickWand, readImageBlob, withMagickWandGenesis)
import Network.Wai.Parse (FileInfo(..), lbsBackEnd, parseRequestBody)
import System.FilePath.Posix (joinPath, takeExtension)
import Types (RequestProcessor, askReq)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- |Post a new thread, returning the file and post IDs on success.
-- Todo: Take the board name, and produce a nice error when it doesn't
-- exist.
-- Todo: Use the error monad, and return good error messages on
-- failure
newThread :: BoardId -- ^ The board
          -> RequestProcessor (Maybe (FileId, PostId))
newThread board = do
  thread <- handlePostForm board Nothing
  case thread of
    Just (Just f, p) -> return $ Just (f, p)
    _ -> return Nothing

-- |Post a new reply, returning the file (if it exists) and post IDs
-- on success.
-- Todo: Take the board name and thread number, and produce nice
-- errors when they don't exist.
-- Todo: Use the error monad, and return good error messages on
-- failure.
newReply :: BoardId -- ^ The board
         -> PostId  -- ^ The OP
         -> RequestProcessor (Maybe (Maybe FileId, PostId))
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
               -> RequestProcessor (Maybe (Maybe FileId, PostId))
handlePostForm boardId threadId = do
  request <- askReq
  (params, files) <- liftIO $ parseRequestBody lbsBackEnd request

  let name     = decodeUtf8 <$> lookup "name"     params
  let email    = decodeUtf8 <$> lookup "email"    params
  let subject  = decodeUtf8 <$> lookup "subject"  params
  let comment  = decodeUtf8 <$> lookup "comment"  params
  let spoiler  = decodeUtf8 <$> lookup "spoiler"  params
  let password = decodeUtf8 <$> lookup "password" params
  let file     = lookup "file" files

  -- If this is a new thread, there must be both an image and a comment.
  -- If this is a reply, there must be at least one of an image or a comment.
  let isValidPost = case threadId of
                      Just _  -> hasValue comment || hasContent file
                      Nothing -> hasValue comment && hasContent file
  
  if isValidPost
    then do
    -- All looks good, construct a post, save the file, and bump the thread.
    board  <- fromJust <$> get boardId
    fileId <- handleFileUpload board file $ isJust spoiler
    postId <- handleNewPost boardId threadId name email subject comment fileId password

    -- bump the thread if there is a thread to bump
    return () `maybe` bumpThread $ threadId

    -- return the relevant information
    return $ Just (fileId, postId)

    -- post was not valid, return Nothing
    else return Nothing
           
-------------------------

-- |Check if a value is set and is nonempty
hasValue :: Maybe Text -> Bool
hasValue Nothing = False
hasValue (Just t) = not $ null (strip t)

-- |Check if a file is nonempty
hasContent :: Maybe (FileInfo BL.ByteString) -> Bool
hasContent Nothing = False
hasContent (Just (FileInfo _ _ c)) = not $ BL.null c

-- |Upload a possible file, returning the ID
-- Todo: Generate thumbnails
-- Todo: Implement maximum file sizes
handleFileUpload :: Board -- ^ The board
                 -> Maybe (FileInfo BL.ByteString) -- ^ The file
                 -> Bool  -- ^ Whether it is spoilered
                 -> RequestProcessor (Maybe FileId)
handleFileUpload board (Just (FileInfo fname _ content)) spoiler = do
  -- Construct the target file path
  fileroot <- conf' "server" "file_root"
  let fname' = map (chr . fromIntegral) $ B.unpack fname
  let fnamehash = md5s (Str fname') ++ takeExtension fname'
  let path = joinPath [fileroot, unpack $ boardName board, "src", fnamehash]
  let size = fromIntegral $ BL.length content

-- I am saddened that this is how to check if a file was
-- uploaded. Also, this should probably be moved to handlePostForm
-- when I refactor this horrible mess.
  if BL.null content
  then return Nothing
  else do
    -- Save the file
    liftIO $ BL.writeFile path content

    -- Get its dimensions
    (width, height) <- liftIO . withMagickWandGenesis $ do
      (_, w) <- magickWand
      readImageBlob w (concat $ BL.toChunks content)
      width  <- getImageWidth w
      height <- getImageHeight w

      return (width, height)

    fmap Just . insert $ File (pack fnamehash) (pack fname') size width height spoiler

handleFileUpload _ Nothing _ = return Nothing

-- |Construct and insert a new post into the database
handleNewPost :: BoardId      -- ^ The board
              -> Maybe PostId -- ^ The OP (if a reply)
              -> Maybe Text   -- ^ The name
              -> Maybe Text   -- ^ The email
              -> Maybe Text   -- ^ The subject
              -> Maybe Text   -- ^ The comment
              -> Maybe FileId -- ^ The file ID
              -> Maybe Text   -- ^ The password
              -> RequestProcessor PostId
handleNewPost boardId threadId name email subject comment fileId password = do
  number  <- ((+1) . length) <$> selectList [PostBoard ==. boardId] []
  updated <- liftIO getCurrentTime

  let name'     = fromMaybe "" name
  let email'    = fromMaybe "" email
  let subject'  = fromMaybe "" subject
  let comment'  = fromMaybe "" comment
  let password' = fromMaybe "" password

  insert $ Post number boardId threadId updated fileId name' email' subject' comment' password'

-- |Bump a thread
bumpThread :: PostId -- ^ The OP
           -> RequestProcessor ()
bumpThread threadId = do
  now <- liftIO getCurrentTime
  update threadId [PostUpdated =. now]
  return ()
