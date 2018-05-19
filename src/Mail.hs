{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Mail where

import           Network.Mail.Mime  hiding (htmlPart)
import qualified Network.Mail.Mime.SES as SES
import           Network.Mail.SMTP (sendMailWithLogin')
import           Network.Mail.SMTP.Auth (UserName, Password)
import           Network.Socket (HostName, PortNumber)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Shakespeare.Text (stext)
import           Text.Hamlet (shamlet)
import           Data.Text
import qualified Data.Text.Lazy.Encoding as Enc.Lazy (encodeUtf8)
import qualified Data.Text.Encoding as Enc.Strict (encodeUtf8)



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


sendMail :: MailSettings -> Mail -> IO ()
sendMail (SendMailSettings _ _) mail = renderSendMail mail
sendMail (GMailSettings _ _ user pass) mail = sendMailWithLogin' host port user pass mail
        where
                host = "smtp.google.com" :: HostName
                port_tls = 587 :: PortNumber
                port_ssl = 465 :: PortNumber
                port = port_tls
sendMail sesSettings@(SesSettings _ _ _ _ _ _) mail 
        = (flip SES.renderSendMailSESGlobal) mail
        $ SES.SES
                { SES.sesFrom         = Enc.Strict.encodeUtf8 . addressEmail $ mailOrigin sesSettings
                , SES.sesTo           = Prelude.map (Enc.Strict.encodeUtf8 . addressEmail) $ mailTo mail
                , SES.sesAccessKey    = Enc.Strict.encodeUtf8 $ sesAccessKey sesSettings
                , SES.sesSecretKey    = Enc.Strict.encodeUtf8 $ sesSecretKey sesSettings
                , SES.sesSessionToken = fmap Enc.Strict.encodeUtf8 $ sesSessionToken sesSettings
                , SES.sesRegion       = sesRegion sesSettings
                }


mkMail :: MailSettings -> Address -> Text -> Text -> Mail
mkMail mailSettings to participant match 
        = (emptyMail $ mailOrigin mailSettings)
                { mailTo = [to]
                , mailHeaders =
                        [ ("Subject", mailSubject mailSettings)
                        ]
                , mailParts = [[textPart, htmlPart]]
                }
        where
                textPart = mkTextPart participant match
                htmlPart = mkHtmlPart participant match

mkTextPart :: Text -> Text -> Part
mkTextPart participant match = Part
        { partType = "text/plain; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partContent = Enc.Lazy.encodeUtf8
                [stext|
                        Hi #{participant}
                        You are Secret Santa for: #{match}!
                |]
        , partHeaders = []
        }

mkHtmlPart :: Text -> Text -> Part
mkHtmlPart participant match = Part
        { partType = "text/html; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partContent = renderHtml
                [shamlet|
<p>Hi #{participant}
<p>You are Secret Santa for: <b>#{match}<\b>!
|]
        , partHeaders = []
        }


