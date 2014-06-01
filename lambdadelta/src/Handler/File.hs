module Handler.File ( static
                    , banner) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, unpack)
import Routes (Sitemap)
import System.Directory (getDirectoryContents)
import System.FilePath.Posix (joinPath)
import System.Random (randomRIO)
import Web.Seacat (Handler, conf')
import Web.Seacat.RequestHandler (respondFile)

-- |Process a request for a static file
-- This isn't the best way to serve static files, your actual web
-- server should really do this.
static :: Handler Sitemap -- ^ 404 handler
       -> [Text]          -- ^ The file path components
       -> Handler Sitemap
static on404 path = respondFile on404 . joinPath $ map unpack path

-- |Return a random banner
banner :: Handler Sitemap
banner = do
  fileroot   <- conf' "server" "file_root"
  banner_dir <- conf' "board" "banner_dir"

  banners <- liftIO . getDirectoryContents $ joinPath [fileroot, banner_dir]
  let banners' = filter (`notElem` [".", ".."]) banners
  banner <- choose banners'

  respondFile undefined $ joinPath [banner_dir, banner]

-- | Select a random element from a list
choose :: MonadIO m
       => [a] -- ^ The list
       -> m a
choose xs = do
  idx <- liftIO $ randomRIO (0, length xs - 1)
  return $ xs !! idx