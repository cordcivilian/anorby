{-# LANGUAGE OverloadedStrings #-}

module Utils.Email where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified System.Environment as Env
import qualified Network.Mail.Mime as Mail
import qualified Network.Mail.SMTP as SMTP

import Utils.Config

-- | Email Configuration

data EmailConfig = EmailConfig
  { emailHost :: String
  , emailPort :: Int
  , emailUser :: String
  , emailPass :: String
  , emailFrom :: T.Text
  , emailSend :: Bool
  , emailBaseUrl :: T.Text
  }

getEmailConfig :: IO EmailConfig
getEmailConfig = do
  config <- getConfig
  pass <- maybe "" id <$> Env.lookupEnv "SMTP"
  return $ EmailConfig
    { emailHost = "smtp.protonmail.ch"
    , emailPort = 587
    , emailUser = "one@walden99.com"
    , emailPass = pass
    , emailFrom = "one@walden99.com"
    , emailSend = environment config == Production
    , emailBaseUrl =
        if environment config == Production then
          "https://anorby.walden99.com"
        else
          "http://localhost:5001"
    }

-- | Email Sending Functions

sendAuthEmail :: EmailConfig -> T.Text -> T.Text -> IO ()
sendAuthEmail config toEmail hash = do
  let subject = "Your Anorby Authentication Link"
      authUrl = emailBaseUrl config <> "/auth/" <> hash
      textBody = TL.fromStrict $
                 "Click this link to authenticate: " <> authUrl
      htmlBody = TL.fromStrict $
                 "<p>Click this link to authenticate: \
                 \<a href=\"" <> authUrl <> "\">" <> authUrl <> "</a></p>"

      from = Mail.Address (Just "anorby") (emailFrom config)
      to = Mail.Address Nothing toEmail

  if emailSend config then do
    mail <- Mail.simpleMail from to "" "" subject
      [(TL.toStrict textBody, "plain"), (TL.toStrict htmlBody, "html")]
    SMTP.sendMailWithLogin
      (emailHost config)
      (emailUser config)
      (emailPass config)
      mail
  else do
    putStrLn $ "auth url for " ++ T.unpack toEmail ++ ": " ++ T.unpack authUrl

sendLogoutConfirmEmail :: EmailConfig -> T.Text -> T.Text -> IO ()
sendLogoutConfirmEmail config toEmail token = do
  let subject = "Confirm Anorby Logout"
      confirmUrl = emailBaseUrl config <> "/logout/confirm?token=" <> token
      textBody = TL.fromStrict $
                 "Click this link to confirm logout from all devices: "
                 <> confirmUrl
      htmlBody = TL.fromStrict $
                 "<p>Click this link to confirm logout from all devices: \
                 \<a href=\"" <> confirmUrl <> "\">"
                 <> confirmUrl <> "</a></p>"

      from = Mail.Address (Just "anorby") (emailFrom config)
      to = Mail.Address Nothing toEmail

  if emailSend config then do
    mail <- Mail.simpleMail from to "" "" subject
      [(TL.toStrict textBody, "plain"), (TL.toStrict htmlBody, "html")]
    SMTP.sendMailWithLogin
      (emailHost config)
      (emailUser config)
      (emailPass config)
      mail
  else do
    putStrLn $ "logout confirm url for " ++ T.unpack toEmail ++ ": " ++ T.unpack confirmUrl

sendDeleteConfirmEmail :: EmailConfig -> T.Text -> T.Text -> IO ()
sendDeleteConfirmEmail config toEmail token = do
  let subject = "Confirm Anorby Account Deletion"
      confirmUrl = emailBaseUrl config <> "/delete/confirm?token=" <> token
      textBody = TL.fromStrict $
                 "Click this link to permanently delete your account: "
                 <> confirmUrl
      htmlBody = TL.fromStrict $
                 "<p>Click this link to permanently delete your account: \
                 \<a href=\"" <> confirmUrl <> "\">"
                 <> confirmUrl <> "</a></p>"

      from = Mail.Address (Just "anorby") (emailFrom config)
      to = Mail.Address Nothing toEmail

  if emailSend config then do
    mail <- Mail.simpleMail from to "" "" subject
      [(TL.toStrict textBody, "plain"), (TL.toStrict htmlBody, "html")]
    SMTP.sendMailWithLogin
      (emailHost config)
      (emailUser config)
      (emailPass config)
      mail
  else do
    putStrLn $ "delete confirm url for " ++ T.unpack toEmail ++ ": " ++ T.unpack confirmUrl
