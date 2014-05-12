{-# LANGUAGE OverloadedStrings, Rank2Types, TypeFamilies #-}

module Browse.User (index, board, thread, Browse.User.postThread, postReply) where

import Browse
import Browse.Error (error400, error404)
import Control.Monad.IO.Class (liftIO)
import Configuration (conf')
import Data.Char (chr)
import Data.Hash.MD5 (Str(..), md5s)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Database
import Database.Persist
import Data.Text (Text)
import Graphics.ImageMagick.MagickWand
import Network.Wai.Parse
import System.FilePath.Posix (joinPath, takeExtension)
import Types

import qualified Browse.Templates as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as Te
import qualified Database as D
import qualified Routes as R

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

-- |Handle a request to post a new thread
-- Todo: anti-spam
-- Todo: Only respond to POST
-- Todo: Handle noko
-- Todo: Better error messages
postThread :: Text -> Handler
postThread board = do
  maybeBoard <- getBy $ UniqueBoardName board

  case maybeBoard of
    Just (Entity boardId _) -> do
      result <- handlePostForm boardId Nothing
      case result of
        Just _ -> redirect $ R.Board board 1
        Nothing -> error400 "Failed to create post."

    Nothing -> error404 "No such board"

postReply :: Text -> Int -> Handler
postReply board thread = utf8200Response "post reply"

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

handlePostForm :: D.BoardId      -- ^ The board
               -> Maybe D.PostId -- ^ The OP
               -> RequestProcessor (Maybe (Maybe FileId, PostId))
handlePostForm boardId threadId = do
  request <- askReq
  (params, files) <- liftIO $ parseRequestBody lbsBackEnd request

  let name     = fmap decodeUtf8 $ lookup "name"     params
  let email    = fmap decodeUtf8 $ lookup "email"    params
  let subject  = fmap decodeUtf8 $ lookup "subject"  params
  let comment  = fmap decodeUtf8 $ lookup "comment"  params
  let file     = lookup "file" files
  let spoiler  = fmap decodeUtf8 $ lookup "spoiler"  params
  let password = fmap decodeUtf8 $ lookup "password" params

  -- If this is a new thread, there must be both an image and a
  -- comment
  if isJust threadId && (not (hasValue comment) || not (hasContent file))
  then return Nothing

  -- If this is a reply, there must be at least one of an image or a
  -- comment
  else if isNothing threadId && not (hasValue comment) && not (hasContent file)
       then return Nothing

  -- All looks good, construct a post, save the file, and bump the
  -- thread
       else do board  <- fmap fromJust $ get boardId
               fileId <- handleFileUpload board file $ isJust spoiler
               postId <- handleNewPost boardId threadId name email subject comment fileId password

               case threadId of
                 Just tid -> bumpThread tid
                 Nothing -> return $ ()
               return $ Just (fileId, postId)

-- |Check if a value is set and is nonempty
hasValue :: Maybe Text -> Bool
hasValue Nothing = False
hasValue (Just t) = Te.length (Te.strip t) /= 0

-- |Check if a file is nonempty
hasContent :: Maybe (FileInfo L.ByteString) -> Bool
hasContent Nothing = False
hasContent (Just (FileInfo _ _ c)) = L.length c /= 0

-- |Upload a possible file, returning the ID
-- Todo: Generate thumbnails
-- Todo: Implement maximum file sizes
handleFileUpload :: D.Board -- ^ The board
                 -> Maybe (FileInfo L.ByteString) -- ^ The file
                 -> Bool -- ^ Whether it is spoilered
                 -> RequestProcessor (Maybe FileId)
handleFileUpload board (Just (FileInfo fname _ content)) spoiler = do
  -- Construct the target file path
    fileroot <- conf' "server" "file_root"
    let fname' = map (chr . fromIntegral) $ B.unpack fname
    let fnamehash = md5s (Str fname') ++ takeExtension fname'
    let path = joinPath [fileroot, Te.unpack $ boardName board, "src", fnamehash]
    let size = fromIntegral $ L.length content

-- I am saddened that this is how to check if a file was
-- uploaded. Also, this should probably be moved to handlePostForm
-- when I refactor this horrible mess.
    if size == 0
    then return Nothing
    else do
      -- Save the file
      liftIO $ L.writeFile path content

      -- Get its dimensions
      (width, height) <- liftIO . withMagickWandGenesis $ do
        (_, w) <- magickWand
        readImageBlob w (B.concat $ L.toChunks content)
        width  <- getImageWidth w
        height <- getImageHeight w

        return (width, height)

      fmap Just . insert $ D.File (Te.pack fnamehash) (Te.pack fname') size width height spoiler

handleFileUpload _ Nothing _ = return Nothing

-- |Construct and insert a new post into the database
handleNewPost :: D.BoardId      -- ^ The board
              -> Maybe D.PostId -- ^ The OP (if a reply)
              -> Maybe Text     -- ^ The name
              -> Maybe Text     -- ^ The email
              -> Maybe Text     -- ^ The subject
              -> Maybe Text     -- ^ The comment
              -> Maybe FileId   -- ^ The file ID
              -> Maybe Text     -- ^ The password
              -> RequestProcessor PostId
handleNewPost boardId threadId name email subject comment fileId password = do
    number  <- fmap ((+1) . length) $ selectList [PostThread ==. threadId] []
    updated <- liftIO $ getCurrentTime

    let name'     = fromMaybe "" name
    let email'    = fromMaybe "" email
    let subject'  = fromMaybe "" subject
    let comment'  = fromMaybe "" comment
    let password' = fromMaybe "" password

    insert $ D.Post number boardId threadId updated fileId name' email' subject' comment' password'

-- |Bump a thread
bumpThread :: D.PostId -- ^ The OP
           -> RequestProcessor ()
bumpThread threadId = do now <- liftIO $ getCurrentTime
                         update threadId [PostUpdated =. now]
                         return ()

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
       let omittedImages  = imageReplies - length (filter (isJust . fst) posts')

       return $ T.TThread opFile op omittedReplies omittedImages posts'


-- |Get the image for a post
getPostImage :: Entity Post -- ^ The post
             -> RequestProcessor (Maybe D.File, Post)
getPostImage (Entity _ post) = case postFile post of
                                 Nothing -> return (Nothing, post)
                                 Just fileid -> do file <- get fileid
                                                   return (file, post)
