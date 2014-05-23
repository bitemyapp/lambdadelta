{-# LANGUAGE OverloadedStrings #-}

module Web.Seacat.RequestHandler.Middleware ( on
                                            , onGet
                                            , onPost
                                            , rateLimit) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Either.Utils (forceEither)
import Data.Time.Clock (NominalDiffTime, addUTCTime, getCurrentTime)
import Database.Persist
import Network.HTTP.Types.Method (StdMethod(..), parseMethod)
import Network.Wai (requestMethod, remoteHost)
import Web.Routes.PathInfo (PathInfo)
import Web.Seacat.Database.Internal
import Web.Seacat.RequestHandler.Types (Handler, askReq)

-- |Run the provided handler on a GET request
onGet :: PathInfo r
      => Handler r -- ^ The 405 handler
      -> Handler r -- ^ The handler
      -> Handler r
onGet = on GET

-- |Run the provided handler on a Post request
onPost :: PathInfo r
       => Handler r -- ^ The 405 handler
       -> Handler r -- ^ The handler
       -> Handler r
onPost = on POST

-- |Run the provided handler if the HTTP method matches, running the
-- 405 error handler if not.
on :: PathInfo r
   => StdMethod -- ^ The method to run on
   -> Handler r -- ^ The 405 handler
   -> Handler r -- ^ The handler
   -> Handler r
on method on405 handler = do
  req <- askReq
  let rmethod = forceEither . parseMethod . requestMethod $ req
  if rmethod == method
  then handler
  else on405

--------------------

-- |Rate limit a particular route. This only looks at individual IPs,
-- and not ranges.
rateLimit :: PathInfo r
          => Maybe String -- ^ The tag of the route, used to
                         -- distinguish them. `Nothing` matches all
                         -- rate-limited routes, `Just x` matches only
                         -- routes tagged with `x`.  This allows a
                         -- person to be limited on one route but not
                         -- another.
          -> NominalDiffTime -- ^ How frequently an individual can access
                            -- this route.
          -> Handler r -- ^ The handler to use if limited.
          -> Handler r -- ^ The handler
          -> Handler r
rateLimit tag freq onLimit handler = do
  ip <- (show . remoteHost) <$> askReq

  now <- liftIO $ getCurrentTime

  deleteWhere [IPBanExpires <. now]

  ban <- selectFirst ([ IPBanApplies ==. tag
                     , IPBanTarget  ==. ip
                     ] ||.
                     [ IPBanApplies ==. Nothing
                     , IPBanTarget  ==. ip
                     ]) []

  case ban of
    Just _  -> onLimit
    Nothing -> do
      let banUntil = addUTCTime freq now
      insert $ IPBan tag banUntil "Automatic rate limiting ban" ip
      handler
