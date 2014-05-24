{-# LANGUAGE OverloadedStrings #-}

module Web.Seacat.RequestHandler.BanHammer ( ipBan
                                           , rateLimit) where

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
import Web.Seacat.Database.Internal
import Web.Seacat.RequestHandler.Types (Handler, askReq)

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

  now <- liftIO getCurrentTime

  deleteWhere [RateLimitExpires <. now]

  ban <- selectFirst ([ RateLimitApplies ==. tag
                     , RateLimitTarget  ==. ip
                     ] ||.
                     [ RateLimitApplies ==. Nothing
                     , RateLimitTarget  ==. ip
                     ]) []

  case ban of
    Just _  -> onLimit
    Nothing -> do
      let banUntil = addUTCTime freq now
      insert $ RateLimit tag banUntil ip
      handler

--------------------

-- |Check if someone is banned, and send them to the error handler if
-- not. This uses the same route distinguishing method as rate limited
-- routes. The error handler takes as a parameter the expiration time
-- and reason.
ipBan :: PathInfo r => Maybe String -> (UTCTime -> Text -> Handler r) -> Handler r -> Handler r
ipBan tag onBan handler = do
  ip <- remoteHost <$> askReq

  now <- liftIO getCurrentTime

  deleteWhere [IPBanExpires <. now]
  deleteWhere [IPRangeBanExpires <. now]

  ban <- selectFirst ([ IPBanApplies ==. tag
                     , IPBanTarget  ==. show ip
                     ] ||.
                     [ IPBanApplies ==. Nothing
                     , IPBanTarget  ==. show ip
                     ]) []

  let intIp = case ip of
                SockAddrInet _ hostaddr -> fromIntegral hostaddr
                SockAddrInet6 _ _ (a, b, c, d) _ -> let a'  = shift (fromIntegral a :: Integer) 96
                                                        b' = shift (fromIntegral b :: Integer) 64
                                                        c' = shift (fromIntegral c :: Integer) 32
                                                        d' = fromIntegral d
                                                    in fromIntegral $ a' .|. b' .|. c' .|. d'
                SockAddrUnix _ -> -1

  rangeBan <- selectFirst ([ IPRangeBanApplies ==. tag
                          , IPRangeBanStart <=. intIp
                          , IPRangeBanStop >=. intIp
                          ] ||.
                          [ IPRangeBanApplies ==. Nothing
                          , IPRangeBanStart <=. intIp
                          , IPRangeBanStop >=. intIp
                          ]) []
  case ban of
    Just (Entity _ ipban) -> onBan (iPBanExpires ipban) (iPBanReason ipban)
    Nothing -> case rangeBan of
                Just (Entity _ rangeban) -> onBan (iPRangeBanExpires rangeban) (iPRangeBanReason rangeban)
                Nothing -> handler
