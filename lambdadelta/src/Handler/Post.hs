{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Handler.Post ( Target(..)
                    , newThread
                    , newReply) where

import Prelude hiding (concat, null)

import Control.Applicative ((<$>))
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Error (ErrorT, throwError)
import Data.ByteString (concat)
import Data.Char (chr)
import Data.Hash.MD5 (Str(..), md5s)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text, null, pack, splitOn, strip, unpack)
import Data.Text.Lazy (toStrict)
import Data.Time.Clock (getCurrentTime)
import Database
import Database.Persist
import Graphics.ImageMagick.MagickWand
import Handler.Admin (bump, deleteThread)
import Routes (Sitemap)
import System.FilePath.Posix (joinPath, takeExtension)
import Text.Blaze.Html (Html, toHtml, preEscapedToHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Seacat (ConfigParser, FileInfo(..), RequestProcessor, askConf, conf', get', files, param', hasParam)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

data Target = Index | Thread
              deriving Eq

data APost = APost { _name     :: Text
                   , _email    :: Text
                   , _subject  :: Text
                   , _comment  :: Html
                   , _password :: Text
                   , _file     :: Maybe (FileInfo BL.ByteString)
                   , _spoiler  :: Bool
                   , _bump     :: Bool
                   , _target   :: Target
                   }

-- |Post a new thread, returning the file and post IDs on success.
-- Todo: Take the board name, and produce a nice error when it doesn't
-- exist.
newThread :: BoardId -- ^ The board
          -> ErrorT String (RequestProcessor Sitemap) (Target, FileId, Int)
newThread board = do
  (t, Just f, p) <- handlePostForm board Nothing

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

  return (t, f, p)

-- |Post a new reply, returning the file (if it exists) and post IDs
-- on success.
-- Todo: Take the board name and thread number, and produce nice
-- errors when they don't exist.
newReply :: BoardId -- ^ The board
         -> PostId  -- ^ The OP
         -> ErrorT String (RequestProcessor Sitemap) (Target, Maybe FileId, Int)
newReply board = handlePostForm board . Just

-------------------------

-- |Handle a post form, saving the post (and optional file) into the
-- database, returning the IDs. This returns Nothing if something went
-- wrong.
-- Todo: Use digestive-functors
-- Todo: Store temporary files on disk, not in memory
-- Todo: tripcodes - come up with a good way for the name field to
-- modify the post
handlePostForm :: BoardId      -- ^ The board
               -> Maybe PostId -- ^ The OP
               -> ErrorT String (RequestProcessor Sitemap) (Target, Maybe FileId, Int)
handlePostForm boardId threadId = do
  conf <- lift askConf

  fs   <- lift files
  post <- lift . fmap (preprocess conf) . makePost $ lookup "file" fs

  -- If this is a new thread, there must be both an image and a comment.
  -- If this is a reply, there must be at least one of an image or a comment.
  let isValidPost = case threadId of
                      Just _  -> hasComment post || hasFile post
                      Nothing -> hasComment post && hasFile post
  
  -- Return an error if the post isn't good
  unless isValidPost $ case threadId of
                         Just _  -> replyError
                         Nothing -> threadError post

  -- All looks good, construct a post, save the file, and bump the thread.
  lift $ commitPost boardId threadId post

-- |Extract a post from the POST!
makePost :: Maybe (FileInfo BL.ByteString)
           -- ^ The possible file (careful: being a Just here doesn't
           -- make it necessarily valid)
         -> RequestProcessor Sitemap APost
makePost thefile = do
  name     <- param' "name"     ""
  email    <- param' "email"    ""
  subject  <- param' "subject"  ""
  comment  <- param' "comment"  ""
  password <- param' "password" ""
  spoiler  <- hasParam "spoiler"

  let file = case thefile of
               Just f@(FileInfo _ _ c) -> if BL.null c
                                          then Nothing
                                          else Just f
               _ -> Nothing

  return $ APost name email subject (toHtml comment) password file spoiler True Index

-- |Commit a new post and possible file upload to the database
commitPost :: BoardId      -- ^ The board
           -> Maybe PostId -- ^ The OP
           -> APost        -- ^ The post
           -> RequestProcessor Sitemap (Target, Maybe FileId, Int)
commitPost boardId threadId post = do
  board  <- fromJust <$> get boardId
  fileId <- case _file post of
             Just f -> Just <$> handleFileUpload board f (_spoiler post)
             _ -> return Nothing
  (_, postNumber) <- handleNewPost boardId threadId post fileId

  -- bump the thread if there is a thread to bump
  when (_bump post)$ return () `maybe` bump $ threadId

  -- return the relevant information
  return (_target post, fileId, postNumber)

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
              -> APost        -- ^ The post
              -> Maybe FileId -- ^ The file ID
              -> RequestProcessor Sitemap (PostId, Int)
handleNewPost boardId threadId post fileId = do
  number  <- ((+1) . length) <$> selectList [PostBoard ==. boardId] []
  updated <- liftIO getCurrentTime

  pId <- insert $ Post number
                      boardId
                      threadId
                      updated
                      updated
                      fileId
                      (_name post)
                      (_email post)
                      (_subject post)
                      (toStrict . renderHtml $ _comment post)
                      (_password post)

  return (pId, number)

-------------------------

-- |Transform a post according to arbitrary functions. These functions
-- should NOT call any HTML escaping functions on the comment, as
-- otherwise we'll end up with really escaped HTML, and interference.
preprocess :: ConfigParser -- ^ The configuration
           -> APost        -- ^The original post
           -> APost
preprocess c = doNoko c . doSage c . doName c . doLinebreaks c

-- |Turn linebreaks into <br>s in the comment
doLinebreaks :: a -> APost -> APost
doLinebreaks _ post = post { _comment = preEscapedToHtml . T.replace "\n" "<br>" . toText $ _comment post }
    where toText = toStrict . renderHtml

-- |Turn an empty name into "Anonymous"
doName :: a -> APost -> APost
doName _ post = if null . strip $ _name post
                then post { _name = "Anonymous" }
                else post

-- |Don't bump the thread if "sage" is in the email field, and sage is allowed
doSage :: ConfigParser -> APost -> APost
doSage c post | get' c "board" "allow_sage" && inEmail post "sage" = post { _bump = False }
              | otherwise = post

-- |Go straight to the thread page if "noko" is in the email field, and noko is allowed
doNoko :: ConfigParser -> APost -> APost
doNoko c post | get' c "board" "always_noko" || (get' c "board" "allow_noko" && inEmail post "noko") = post { _target = Thread }
              | otherwise = post

-------------------------

-- |Check if a post has a comment
hasComment :: APost -> Bool
hasComment = not . null . strip . toStrict . renderHtml . _comment

-- |Check if a post has a file
hasFile :: APost -> Bool
hasFile = isJust . _file

-- |Check if a word is in the email
inEmail :: APost -> Text -> Bool
inEmail post search = search `elem` splitOn " " (_email post)

-------------------------

-- |Get the error message for a reply
replyError :: Monad m => ErrorT String m a
replyError = throwError "Replies must have at least a file or a comment"

-- |Get the error message for a thread
threadError :: Monad m
            => APost -- ^ The post
            -> ErrorT String m a
threadError post = throwError threadError'
  where threadError' | not (hasComment post) && not (hasFile post) = "Topics must have both a file and a comment."
                     | not (hasComment post) = "Topics must have a comment"
                     | not (hasFile post) = "Topics must have a file."
