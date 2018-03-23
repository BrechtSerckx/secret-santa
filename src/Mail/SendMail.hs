module Mail.SendMail where

import           Network.Mail.Mime  (Mail,renderSendMail)
import Mail (Mailer,sendMail)

instance Mailer SendMailMailer where
    sendMail = sendMail'

sendMail' :: SendMailMailer -> Mail -> IO ()
sendMail' _ mail = renderSendMail mail


data SendMailMailer = SendMailMailer
