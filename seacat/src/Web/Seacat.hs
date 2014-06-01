-- |The entry point of the Seacat module hierarchy: this module
-- re-exports many common functions and types from the rest of the
-- library, and submodules of this comprise all Seacat functionality.
module Web.Seacat
    ( -- * Types from dependencies, re-exported for convenience.
      FileInfo(..)
    , PathInfo(..)
    , ConfigParser(..)

    -- * Running the server
    , SeacatSettings(..)
    , defaultSettings
    , seacat

    -- * Request handler types
    , Cry(..)
    , MkUrl
    , RequestProcessor
    , Handler

    -- * Request handler utilities
    , askCry
    , askConf
    , askMkUrl
    , askReq

    -- * Configuration accessors
    , get
    , get'
    , conf
    , conf'

    -- * Response builders
    , htmlResponse
    , htmlResponse'
    , textResponse
    , textResponse'
    , respondFile
    , redirect

    -- * Parameter accessors
    , param
    , param'
    , hasParam
    , params
    , files
    ) where

import Web.Seacat.Configuration
import Web.Seacat.Server
import Web.Seacat.RequestHandler
import Web.Seacat.RequestHandler.Types
