{-# LANGUAGE OverloadedStrings #-}
module Mail where

import Network.Mail.Mime (Mail, Part,Address)
import Network.Mail.SMTP (sendMailWithLogin', plainTextPart, simpleMail)
import Network.Mail.SMTP.Auth (UserName)
import Network.Mail.SMTP.Types 
import Data.Text.Lazy (Text)

from = Address (Just "Secret Santa" ) "brecht_serckx@gmail.com" 
cc = []
bcc = []
subject = "Your Secret Santa Match"

host = "smtp.google.com"
port_tls = 587
port_ssl = 465
user = "***REMOVED***"
pass = "secret_password"


mkBody :: Text -> Text -> Part
mkBody participant match = plainTextPart $ 
    "Dear  ++ participant ++ , you are Secret Santa for *match*!"

mkMail to participant match = simpleMail from to cc bcc subject [body]
    where
        body = mkBody participant match

sendGmail :: Mail -> IO ()
sendGmail mail = sendMailWithLogin' host port_tls user pass mail
