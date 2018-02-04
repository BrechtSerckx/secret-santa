module Mail where




import Network.Mail.SMTP

from = Address (Just "Secret Santa") user
cc = []
bcc = []
subject = "Your Secret Santa Match"

host = "smtp.google.com"
port_tls = 587
port_ssl = 465
user = "brecht.serckx@gmail.com"
pass = "secret_password"


mkBody participant match = "Dear " ++ show participant ++ ", you are Secret Santa for " ++ show match ++ "!"

mkMail to participant match = simpleMail from to cc bcc subject [body]
    where
        body = mkBody participant match

sendGmail mail = sendMailWithLogin' host port user pass mail
