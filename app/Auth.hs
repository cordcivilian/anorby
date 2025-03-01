{-# LANGUAGE OverloadedStrings #-}

module Auth where

import qualified Control.Monad as Monad
import qualified Data.Binary.Builder as Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.Word as Word
import qualified System.Environment as Env
import qualified System.Random as Random
import qualified Text.Read as Read
import qualified Web.Cookie as Cookie

import qualified Network.Wai as Wai

import qualified Database.SQLite.Simple as SQL

import Types
import Database

-- | User management

getAuthenticatedUser :: SQL.Connection -> Wai.Request -> IO (Maybe User)
getAuthenticatedUser conn req = do
  case getCookie req of
    Nothing -> return Nothing
    Just cookieBS -> do
      let hash = TE.decodeUtf8 cookieBS
      getUserFromAuthHash conn hash

-- | Token generation and validation

generateAnswerToken :: UserID -> AorbID -> IO T.Text
generateAnswerToken uid aid = do
  maybeSecret <- Env.lookupEnv "ANORBY"
  now <- POSIXTime.getPOSIXTime
  let expiry = now + 300
      tokenData = BSL.concat
        [ Builder.toLazyByteString $ Builder.putWord64host $ fromIntegral uid
        , Builder.toLazyByteString $ Builder.putWord64host $ fromIntegral aid
        , Builder.toLazyByteString $ Builder.putWord64host $ floor expiry
        ]
      secret = BSL.fromStrict $ TE.encodeUtf8 $
        T.pack $ maybe "no-secret" id maybeSecret
      signature = SHA.showDigest $ SHA.hmacSha256 secret tokenData
  return $ T.pack $
    show uid ++ "." ++
    show aid ++ "." ++
    show (floor expiry :: Integer) ++ "." ++
    signature

validateAnswerToken :: T.Text -> UserID -> AorbID -> IO Bool
validateAnswerToken token expectedUid expectedAid = do
  maybeSecret <- Env.lookupEnv "ANORBY"
  now <- POSIXTime.getPOSIXTime
  let parts = T.splitOn "." token
  case parts of
    [uidStr, aidStr, expiryStr, signature] -> do
      case
        ( Read.readMaybe (T.unpack uidStr) :: Maybe Int
        , Read.readMaybe (T.unpack aidStr) :: Maybe Int
        , Read.readMaybe (T.unpack expiryStr) :: Maybe Integer
        ) of
        (Just uid, Just aid, Just expiry) ->
          if uid /= expectedUid || aid /= expectedAid
            then return False
            else if fromIntegral expiry < now
              then return False
              else do
                let tokenData = BSL.concat
                      [ Builder.toLazyByteString $
                        Builder.putWord64host $ fromIntegral uid
                      , Builder.toLazyByteString $
                        Builder.putWord64host $ fromIntegral aid
                      , Builder.toLazyByteString $
                        Builder.putWord64host $ fromIntegral expiry
                      ]
                    secret = BSL.fromStrict $ TE.encodeUtf8 $
                      T.pack $ maybe "no-secret" id maybeSecret
                    expectedSignature =
                      SHA.showDigest $ SHA.hmacSha256 secret tokenData
                return $ signature == T.pack expectedSignature
        _ -> return False
    _ -> return False

generateAuthHash :: T.Text -> IO T.Text
generateAuthHash email = do
  maybeSecret <- Env.lookupEnv "ANORBY"
  now <- POSIXTime.getPOSIXTime
  nonce <- B.pack <$>
    Monad.replicateM 32 (Random.randomRIO (0 :: Word.Word8, 255))
  let secret = BSL.fromStrict $ TE.encodeUtf8 $
                T.pack $ maybe "no-secret" id maybeSecret
      hashData = BSL.concat
        [ secret
        , BSL.fromStrict nonce
        , BSL.fromStrict $ TE.encodeUtf8 email
        , Builder.toLazyByteString $
            Builder.putWord64host $ floor now
        ]
      hash1 = SHA.sha512 hashData
      hash2 = SHA.hmacSha512 secret
        (BSL.fromStrict $ BS.pack $ SHA.showDigest hash1)
  return $ T.pack $ SHA.showDigest hash2

-- | Cookie management

getCookie :: Wai.Request -> Maybe BS.ByteString
getCookie req =
  case lookup "Cookie" (Wai.requestHeaders req) of
    Nothing -> Nothing
    Just cookies ->
      let parsedCookies = Cookie.parseCookies cookies
      in lookup "X-Auth-Hash" parsedCookies

setCookie :: T.Text -> Wai.Response -> Wai.Response
setCookie hash resp =
  let cookie = Cookie.defaultSetCookie
        { Cookie.setCookieName = "X-Auth-Hash"
        , Cookie.setCookieValue = TE.encodeUtf8 hash
        , Cookie.setCookiePath = Just "/"
        , Cookie.setCookieSecure = True
        , Cookie.setCookieSameSite = Just Cookie.sameSiteStrict
        , Cookie.setCookieHttpOnly = True
        , Cookie.setCookieMaxAge = Just (10 * 365 * 24 * 60 * 60)
        }
      cookieHeader = ("Set-Cookie", BSL.toStrict $
                      Builder.toLazyByteString $
                      Cookie.renderSetCookie cookie)
  in Wai.mapResponseHeaders ((:) cookieHeader) resp
