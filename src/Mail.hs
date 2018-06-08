{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mail 
        ( MailSettings(..)
        , sendMail
        , Network.Mail.Mime.Address(..)
        , Network.Mail.Mime.Mail(..)
        , Network.Mail.Mime.Part(..)
        , Network.Mail.Mime.emptyMail
        , Network.Mail.Mime.Encoding(..)
        , Network.Mail.SMTP.Auth.UserName
        , Network.Mail.SMTP.Auth.Password
        )
where

import           Data.Aeson
    ( FromJSON, parseJSON, withObject, (.:), (.:?),Value(..)
    )
import           Data.Aeson.Types            (typeMismatch)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)

import           Network.Mail.Mime (Address(..),Mail(..),renderSendMail,Part(..),emptyMail,Encoding(..))
import qualified Network.Mail.Mime.SES as SES
import           Network.Mail.SMTP (sendMailWithLogin')
import           Network.Mail.SMTP.Auth (UserName, Password)



data MailSettings 
        = SendMailSettings
                { mailOrigin      :: Address
                , mailSubject     :: Text
                }
        | GMailSettings
                { mailOrigin      :: Address
                , mailSubject     :: Text
                , gMailUserName   :: UserName
                , gMailPassword   :: Password
                }
        | SesSettings
                { mailOrigin      :: Address
                , mailSubject     :: Text
                , sesAccessKey    :: Text
                , sesSecretKey    :: Text
                , sesSessionToken :: Maybe Text
                , sesRegion       :: Text
                }


instance FromJSON MailSettings where
    parseJSON = withObject "MailSettings" $ \o -> do
        mailOriginName  <- o .: "origin_name"
        mailOriginEmail  <- o .: "origin_email"
        let mailOrigin = Address (Just mailOriginName) mailOriginEmail
        mailSubject <- o .: "subject"

        svcType :: Text <- o .: "service"
        case svcType of
            "sendmail" -> return SendMailSettings {..}
            "gmail"    -> do
                gMailUserName :: UserName <- o .: "gmail_username"
                gMailPassword :: Password <- o .: "gmail_password"
                return GMailSettings {..}
            "ses"  -> do
                sesAccessKey        :: Text       <- o .:  "ses_access_key"
                sesSecretKey        :: Text       <- o .:  "ses_secret_key"
                sesSessionToken'    :: Maybe Text <- o .:? "ses_session_token"
                sesRegion           :: Text       <- o .:  "ses_region"
                let sesSessionToken = case sesSessionToken' of
                        Nothing -> Nothing
                        Just "" -> Nothing
                        Just _  -> sesSessionToken'
                return SesSettings {..}
            _          -> typeMismatch "MailService" $ Object o


sendMail :: MailSettings -> Mail -> IO ()
sendMail (SendMailSettings _ _) mail = renderSendMail mail
sendMail (GMailSettings _ _ user pass) mail = sendMailWithLogin' host port user pass mail
        where
                host = "smtp.google.com"
                port_tls = 587
                port_ssl = 465
                port = port_tls
sendMail sesSettings@(SesSettings _ _ _ _ _ _) mail 
        = (flip SES.renderSendMailSESGlobal) mail
        $ SES.SES
                { SES.sesFrom         = encodeUtf8 . addressEmail $ mailOrigin sesSettings
                , SES.sesTo           = Prelude.map (encodeUtf8 . addressEmail) $ mailTo mail
                , SES.sesAccessKey    = encodeUtf8 $ sesAccessKey sesSettings
                , SES.sesSecretKey    = encodeUtf8 $ sesSecretKey sesSettings
                , SES.sesSessionToken = fmap encodeUtf8 $ sesSessionToken sesSettings
                , SES.sesRegion       = sesRegion sesSettings
                }


