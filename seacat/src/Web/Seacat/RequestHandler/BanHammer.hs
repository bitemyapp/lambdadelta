{-# LANGUAGE OverloadedStrings #-}

-- |Higher-level handlers to do with banning people.
-- Each of the functions here is of the same basic form, they take a
-- `Tag`, possibly some parameter to determine if a ban or limit is in
-- effect, a handler to apply on the case of ban, and a handler to
-- apply on the case of success.
-- Tags are just a name used to identify a route, they may be shared
-- amongst routes.
module Web.Seacat.RequestHandler.BanHammer ( Tag
                                           , ipBan
                                           , floodProtect
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

-- |A handy name to identify a route.
type Tag = Text

-- |Rate limit a particular route. This only looks at individual IPs,
-- and not ranges.
rateLimit :: PathInfo r
          => Tag
          -> NominalDiffTime
            -- ^ How frequently an individual can access this route.
          -> (UTCTime -> Handler r)
            -- ^ Takes the limit expiration time as a parameter.
          -> Handler r
          -> Handler r
rateLimit tag freq onLimit handler = do
  limited <- isRateLimited tag
  now     <- liftIO getCurrentTime
  ip      <- show . remoteHost <$> askReq

  case limited of
    Just t -> onLimit t
    Nothing -> do
      let banUntil = addUTCTime freq now
      _ <- insert $ SeacatRateLimit tag banUntil ip
      handler

-- |Don't let someone past if they are rate limited, but don't set a
-- new limit if there isn't one already. The parameters are as in
-- `rateLimit`.
rateLimit' :: PathInfo r => Tag -> (UTCTime -> Handler r) -> Handler r -> Handler r
rateLimit' tag onLimit handler = do
  limited <- isRateLimited tag

  case limited of
    Just t -> onLimit t
    Nothing -> handler

-- | Check if an IP is rate limited. If it is, return when the limit
-- ends. This has the side-effect of clearing out all expired limits.
isRateLimited :: PathInfo r => Tag -> RequestProcessor r (Maybe UTCTime)
isRateLimited tag = do
  ip <- show . remoteHost <$> askReq

  now <- liftIO getCurrentTime

  deleteWhere [SeacatRateLimitExpires <. now]

  ban <- selectFirst [ SeacatRateLimitApplies ==. tag
                    , SeacatRateLimitTarget  ==. ip
                    ] []

  return $ (\(Entity _ b) -> seacatRateLimitExpires b) <$> ban

--------------------

-- |Check if someone is banned, and send them to the error handler if
-- so.
ipBan :: PathInfo r
      => Tag
      -> (UTCTime -> Text -> Handler r)
        -- ^ Takes the expiration time and reason of the ban as
        -- parameters.
      -> Handler r
      -> Handler r
ipBan tag onBan handler = do
  ip <- remoteHost <$> askReq

  now <- liftIO getCurrentTime

  deleteWhere [SeacatIPBanExpires <. now]

  ban <- selectFirst [ SeacatIPBanApplies ==. tag
                    , SeacatIPBanStart <=. ipToRational ip
                    , SeacatIPBanStop >=. ipToRational ip
                    ] []
  case ban of
    Just (Entity _ ipban) -> onBan (seacatIPBanExpires ipban) (seacatIPBanReason ipban)
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

--------------------

-- |Flood protect a particular route. This only looks at individual
-- IPs and not ranges. If multiple routes share this tag, they should
-- use the same time period.
floodProtect :: PathInfo r
             => Tag
             -> NominalDiffTime
               -- ^ The time period covered by the protection
             -> Int
               -- ^ How many times in the time period an IP can access
               -- this tag.
             -> Handler r
             -> Handler r
             -> Handler r
floodProtect tag time accesses onFlood handler = do
  ip <- show . remoteHost <$> askReq
  now <- liftIO getCurrentTime

  deleteWhere [SeacatAntiFloodExpires <. now]

  flood <- ((>=accesses) . length) <$> selectList [ SeacatAntiFloodApplies ==. tag
                                                , SeacatAntiFloodTarget  ==. ip
                                                ] []

  if flood
  then onFlood
  else do
    let expires = addUTCTime time now
    _ <- insert $ SeacatAntiFlood tag expires ip
    handler
