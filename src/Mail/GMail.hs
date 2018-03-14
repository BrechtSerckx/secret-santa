module Mail.GMail where
import Network.Mail.Mime
import Network.Mail.SMTP

--instance Mail GMail where
--    sendMail = sendGmail


host = "smtp.google.com"
port_tls = 587
port_ssl = 465
user = "username@mail.com"
pass = "secret_password"


sendGMail :: Mail -> IO ()
sendGMail mail = sendMailWithLogin' host port_tls user pass mail