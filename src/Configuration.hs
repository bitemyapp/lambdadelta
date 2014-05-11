module Configuration (loadConfigFile, defaults, get') where

import Data.ConfigFile
import Data.Either.Utils (forceEither)
import System.IO.Error (catchIOError)

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
loadConfigFile filename = loadCP `catchIOError` const (return Nothing)

    where loadCP = do let base = emptyCP { accessfunc = interpolatingAccess 10 }
                      cp <- readfile base filename
                      return . Just . merge defaults $ forceEither cp

-- |Default configuration values
-- These are for just the settings used by the server
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
           ]

-- |Get a value from the configuration unsafely
get' :: Get_C a => ConfigParser -> SectionSpec -> OptionSpec -> a
get' cp ss os = forceEither $ get cp ss os
