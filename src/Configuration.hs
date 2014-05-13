{-# LANGUAGE FlexibleContexts #-}

module Configuration (ConfigParser,
                      loadConfigFile, defaults,
                      get, get', conf, conf') where

import Control.Applicative ((<$>))
import Control.Monad.Error.Class (MonadError)
import Data.ConfigFile
import Data.Either.Utils (forceEither)
import System.IO.Error (catchIOError)
import Types (RequestProcessor, askConf)
import Web.Routes.PathInfo (PathInfo)

-- |Load a configuration file by name.
-- All errors (syntax, file access, etc) are squashed together,
-- returning a Nothing if anything fails. This is probably ok, given
-- the simplicity of the format, however it may be useful later on to
-- distinguish between syntax errors (and where they are) and access
-- errors, giving the user a better error message than just "oops,
-- something went wrong"
--
-- String interpolation is turned on (with a depth of 10)
--
-- This sets all of the configuration values expected by the main
-- application to their defaults if they weren't present already, but
-- other values are left as they are in the file.
loadConfigFile :: FilePath -> IO (Maybe ConfigParser)
loadConfigFile filename = (Just <$> loadConfigFileUnsafe filename) `catchIOError` const (return Nothing)

-- |Load a configuration file unsafely. This may throw an IO
-- exception.
loadConfigFileUnsafe :: FilePath -> IO ConfigParser
loadConfigFileUnsafe filename = do
  let base = emptyCP { accessfunc = interpolatingAccess 10 }
  cp <- readfile base filename
  return . merge defaults $ forceEither cp

-- |Default configuration values
defaults :: ConfigParser
defaults = forceEither . readstring emptyCP $ unlines
  [ "[server]"
  , "host      = *"
  , "port      = 3000"
  , "web_root  = http://localhost:3000"
  , "file_root = /tmp"
  , "[database]"
  , "connection_string = lambdadelta.sqlite"
  , "pool_size         = 10"
  , "[board]"
  , "board_listing = [[\"b\"]]"
  , "summary_size = 5"
  , "threads_per_page = 10"
  , "maximum_pages = 10"
  , "thumbnail_width = 300"
  , "thumbnail_height = 300"
  , "bump_limit = 300"
  ]

-- |Get a value from the configuration unsafely
get' :: Get_C a => ConfigParser -> SectionSpec -> OptionSpec -> a
get' cp ss os = forceEither $ get cp ss os

-- |Get a value from the configuration in a handler, abstracting the
-- askConf/get pattern
conf :: (Get_C a, MonadError CPError m, PathInfo r) => SectionSpec -> OptionSpec -> RequestProcessor r (m a)
conf ss os = askConf >>= \config -> return $ get config ss os

-- |Get a value from the configuration in a handler unsafely
conf' :: (Get_C a, PathInfo r) => SectionSpec -> OptionSpec -> RequestProcessor r a
conf' ss os = askConf >>= \config -> return $ get' config ss os
