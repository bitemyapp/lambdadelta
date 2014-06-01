{-# LANGUAGE OverloadedStrings #-}

-- |The entry point of the Seacat module hierarchy: this module
-- re-exports many common functions and types from the rest of the
-- library, and submodules of this comprise all Seacat functionality.
module Web.Seacat
    ( -- * Running the server
      SeacatSettings(..)
    , defaultSettings
    , seacat
    ) where

import Web.Seacat.Server