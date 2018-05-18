{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable		 #-}
{-# LANGUAGE FlexibleContexts		   #-}
{-# LANGUAGE GADTs					  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses	  #-}
{-# LANGUAGE OverloadedStrings		  #-}
{-# LANGUAGE QuasiQuotes				#-}
{-# LANGUAGE TemplateHaskell			#-}
{-# LANGUAGE TypeFamilies			   #-}
module Mail where

import           Network.Mail.Mime  hiding (htmlPart)
import           Network.Mail.Mime.SES (SES(..),renderSendMailSESGlobal)
import           Network.Mail.SMTP (sendMailWithLogin')
import           Network.Mail.SMTP.Auth (UserName, Password)
import           Network.Socket (HostName, PortNumber)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Shakespeare.Text (stext)
import           Text.Hamlet (shamlet)
import           Data.ByteString (ByteString)
import qualified Data.Text.Lazy.Encoding
import           Data.Text


from = Address (Just "Secret Santa" ) "secret-santa@secret-santa.net" 
cc = []
bcc = []
subject = "Your Secret Santa Match"


data MailService 
	= SendMailService
	| GMailService
		{ gMailUsername :: UserName
		, gMailPassword :: Password
		}
	| SesMailService
		{ sesMailSes :: SES
		}

data MailServiceSettings 
	= SendMailSettings
	| GMailSettings
		{ gMailSettingsUserName :: UserName
		, gMailSettingsPassword :: Password
		}
	| SesMailSettings
		{ sesMailSettingsFrom :: ByteString
		, sesMailSettingsTo   :: [ByteString]
		, sesAccessKey        :: ByteString
		, sesSecretKey        :: ByteString
		, sesSessionToken     :: Maybe ByteString
		, sesRegion           :: Text
		}

	
makeMailService :: MailServiceSettings -> MailService
makeMailService SendMailSettings = SendMailService
makeMailService (GMailSettings u p) = GMailService u p
makeMailService (SesMailSettings from to aKey sKey token region)
	=
		let ses = SES from to aKey sKey token region
		in  SesMailService ses



sendMail :: MailService -> Mail -> IO ()
sendMail SendMailService mail = renderSendMail mail
sendMail (GMailService user pass) mail = sendMailWithLogin' host port user pass mail
	where
		host = "smtp.google.com" :: HostName
		port_tls = 587 :: PortNumber
		port_ssl = 465 :: PortNumber
		port = port_tls
sendMail (SesMailService ses) mail = renderSendMailSESGlobal ses mail


mkMail :: Address -> Text -> Text -> Mail
mkMail to participant match = (emptyMail $ from)
		{ mailTo = [to]
		, mailHeaders =
			[ ("Subject", subject)
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
	, partContent = Data.Text.Lazy.Encoding.encodeUtf8
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


