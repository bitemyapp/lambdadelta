{-# LANGUAGE OverloadedStrings #-}

-- |Higher-level handlers to do with banning people.
module Web.Seacat.RequestHandler.BanHammer ( ipBan
                                           , rateLimit
                                           , rateLimit') where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.|.), shift)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Word ()
import Database.Persist
import Network.Socket (SockAddr(..))
import Network.Wai (remoteHost)
import Web.Routes.PathInfo (PathInfo)
import Web.Seacat.Database
import Web.Seacat.RequestHandler.Types (Handler, RequestProcessor, askReq)

-- |Rate limit a particular route. This only looks at individual IPs,
-- and not ranges.
rateLimit :: PathInfo r
          => Maybe String -- ^ The tag of the route. This works as in
                         -- `ipBan`.
          -> NominalDiffTime -- ^ How frequently an individual can access
                            -- this route.
          -> (UTCTime -> Handler r)
            -- ^ The handler to use if limited. Takes the limit
            -- expiration time as a parameter
          -> Handler r -- ^ The handler
          -> Handler r
rateLimit tag freq onLimit handler = do
  limited <- isRateLimited tag
  now     <- liftIO getCurrentTime
  ip      <- (show . remoteHost) <$> askReq

  case limited of
    Just t -> onLimit t
    Nothing -> do
      let banUntil = addUTCTime freq now
      _ <- insert $ RateLimit tag banUntil ip
      handler

-- |Don't let someone past if they are rate limited, but don't set a
-- new limit if there isn't one already. The parameters are as in
-- `rateLimit`.
rateLimit' :: PathInfo r => Maybe String -> (UTCTime -> Handler r) -> Handler r -> Handler r
rateLimit' tag onLimit handler = do
  limited <- isRateLimited tag

  case limited of
    Just t -> onLimit t
    Nothing -> handler

-- | Check if an IP is rate limited. If it is, return when the limit
-- ends. This has the side-effect of clearing out all expired limits.
isRateLimited :: PathInfo r => Maybe String -> RequestProcessor r (Maybe UTCTime)
isRateLimited tag = do
  ip <- (show . remoteHost) <$> askReq

  now <- liftIO getCurrentTime

  deleteWhere [RateLimitExpires <. now]

  ban <- selectFirst ([ RateLimitApplies ==. tag
                     , RateLimitTarget  ==. ip
                     ] ||.
                     [ RateLimitApplies ==. Nothing
                     , RateLimitTarget  ==. ip
                     ]) []

  return $ (\(Entity _ b) -> rateLimitExpires b) <$> ban

--------------------

-- |Check if someone is banned, and send them to the error handler if
-- not.
ipBan :: PathInfo r
      => Maybe String
        -- ^ The tag of the route, used to distinguish them. `Nothing`
        -- matches all routes which check bans, `Just x` matches all
        -- routes identified by `x` which check bans. This allows a
        -- person to be banned on one route but not another.
      -> (UTCTime -> Text -> Handler r)
        -- ^ The handler called on a ban. It is given the expiration
        -- time and reason of the ban as parameters.
      -> Handler r -- ^ The handler
      -> Handler r
ipBan tag onBan handler = do
  ip <- remoteHost <$> askReq

  now <- liftIO getCurrentTime

  deleteWhere [IPBanExpires <. now]

  ban <- selectFirst ([ IPBanApplies ==. tag
                     , IPBanStart <=. ipToRational ip
                     , IPBanStop >=. ipToRational ip
                     ] ||.
                     [ IPBanApplies ==. Nothing
                     , IPBanStart <=. ipToRational ip
                     , IPBanStop >=. ipToRational ip
                     ]) []
  case ban of
    Just (Entity _ ipban) -> onBan (iPBanExpires ipban) (iPBanReason ipban)
    Nothing -> handler

-- |Convert an IP address into a Rational, the type I'm using in the
-- database to represent them.
ipToRational :: SockAddr -> Rational
ipToRational (SockAddrInet _ hostAddr) = fromIntegral hostAddr
ipToRational (SockAddrInet6 _ _ (a, b, c, d) _) = let a' = shift (fromIntegral a :: Integer) 96
                                                      b' = shift (fromIntegral b :: Integer) 64
                                                      c' = shift (fromIntegral c :: Integer) 32
                                                      d' = fromIntegral d
                                                  in fromIntegral $ a' .|. b' .|. c' .|. d'
ipToRational (SockAddrUnix _) = -1