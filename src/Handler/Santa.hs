{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE EmptyCase             #-}
module Handler.Santa where


import qualified Data.ByteString.Char8 as BS
import           Data.Either (isLeft,isRight)
import           Data.List (nub,tail)
import           Data.Maybe (fromJust,isJust)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Debug.Trace (trace)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Email.Validate as Email
import           Text.Hamlet (shamlet)
import           Text.Shakespeare.Text

import           Import hiding (count,tail,trace,id,encodeUtf8,multiEmailField)
import qualified Mail (sendMail,MailSettings(..),Address(..),Part(..),Mail(..),emptyMail,Encoding(..))
import qualified SecretSanta


multiForm :: Html -> MForm Handler (FormResult SecretSanta.SantaData, Widget)
multiForm extra = do
        -- description
        let descrFieldSettings = FieldSettings "Description" (Just "Enter a description") (Just "description") (Just "description") [("class","col-xs-12 col-md-9")]
        (descrRes, descrView) <- mopt textareaField descrFieldSettings Nothing
        -- date
        let dateFieldSettings = FieldSettings "Date" (Just "Enter a date") (Just "date") (Just "date") [("class","col-xs-12 col-md-9")]
        date <- utctDay <$> liftIO getCurrentTime
        (dateRes, dateView) <- mopt dayField dateFieldSettings $ Just $ Just $ date
        -- price
        let priceFieldSettings = FieldSettings "Price" (Just "Enter a price") (Just "price") (Just "price") [("class","col-xs-12 col-md-9")]
        (priceRes, priceView) <- mopt doubleField priceFieldSettings $ Just $ Just 5
        -- names
        let namesFieldSettings = FieldSettings "Name" (Just "Enter a names") (Just "names") (Just "names") [("class","col-xs-12 col-sm-10 col-md-3")]
        (namesRes, namesView) <- mreq multiTextField namesFieldSettings Nothing
        -- email
        let emailsFieldSettings = FieldSettings "Email" (Just "Enter a emails") (Just "emails") (Just "emails") [("class","col-xs-12 col-sm-10 col-md-4")]
        (emailsRes, emailsView) <- mreq multiEmailField emailsFieldSettings Nothing

        let widget = $(widgetFile "santa-form")
        let infoRes = mkSantaInfo <$> descrRes <*> dateRes <*> priceRes 
        let psRes = mkSantaParticipants <$> namesRes <*> emailsRes
        let res = case mkSantaData <$> infoRes <*> psRes of
                FormSuccess (Right santaData) -> FormSuccess $ trace (show santaData) santaData
                FormSuccess (Left es)         -> FormFailure es
                _                             -> res
        return (res, widget)


mkSantaData :: Either [Text] SecretSanta.SantaInfo -> Either [Text] [SecretSanta.Participant] -> Either [Text] SecretSanta.SantaData
mkSantaData (Left es) _         = Left es
mkSantaData _         (Left es) = Left es
mkSantaData (Right info) (Right ps) = Right $ SecretSanta.SantaData info ps


mkSantaInfo :: Maybe Textarea -> Maybe Day -> Maybe Double -> Either [Text] SecretSanta.SantaInfo
mkSantaInfo descr day price 
        | isJust price && (fromJust price) < 0 = Left $ return "Price must be positive!"
        | otherwise = Right $ SecretSanta.SantaInfo (unTextarea <$> descr) day price
        

mkSantaParticipants :: [Text] -> [Text] -> Either [Text] [SecretSanta.Participant]
mkSantaParticipants names emails
        | length (nub names) < length names = Left $ return "Your participants must have unique names!"
        | length (nub names) < 2            = Left $ return "You must enter at least two unique participants!"
        | otherwise                         = es
                where
                        ps 
                                = map mkParticipant 
                                . filter (\(name,email) -> name /= "" || email /= "") 
                                $ zip names emails
                        es 
                                | any (isLeft) ps = Left $ lefts ps
                                | otherwise       = Right $ rights $ ps


mkParticipant :: (Text,Text) -> Either Text SecretSanta.Participant
mkParticipant (name,email)
        | name == ""    = Left $ "Name cannot be empty!"
        | email == ""   = Left $ "Email cannot be empty!"
        | otherwise     = Right (name,Mail.Address (Just name) email)



multiTextField :: Field Handler [Text]
multiTextField = Field
        { fieldParse = \rawVals _fileVals -> return $ Right $ Just $ tail rawVals
        , fieldView = \theId name attrs val isReq ->
                [whamlet|
                        <input id="#{theId}" name="#{name}" *{attrs} type=text :isReq:required :(isRight val):data-defaults="#{either id unwords val}">
                |] 
        , fieldEnctype = UrlEncoded
        }

id :: a -> a
id x = x

multiEmailField :: Field Handler [Text]
multiEmailField = Field
        { fieldParse = \rawVals _fileVals -> return $ parseEmails rawVals 
        , fieldView = \theId name attrs val isReq ->
                [whamlet|
                        <input id="#{theId}" name="#{name}" *{attrs} type=email :isReq:required :(isRight val):data-defaults="#{either id unwords val}">
                |]
        , fieldEnctype = UrlEncoded
        }

parseEmails :: [Text] -> Either (SomeMessage (HandlerSite Handler)) (Maybe [Text])
parseEmails es 
        | any (Email.isValid) es' = Right $ Just $ tail es
        | otherwise               = Left $ "Invalid email address!"
                where
                        es' :: [ByteString]
                        es' = map (BS.pack . unpack) es


getSantaR :: Handler Html
getSantaR = do
        (formWidget, formEnctype) <- generateFormPost multiForm
        let infoWidget = [whamlet|
                Please fill in the participant names.
                |]

        let bodyWidget = [whamlet|
                <form method=post action=@{SantaR}#forms enctype=#{formEnctype} .form>
                        ^{formWidget}
                        <button type=button .add_field_button .btn>Add More
                        <button .btn.btn-primary type="submit">
                                Match!
                |]
        defaultLayout $ do
                setTitle "Secret Santa - BrechtSerckx.be"
                $(widgetFile "santa")


postSantaR :: Handler Html
postSantaR = do
        ((result, _formWidget), _formEnctype) <- runFormPost multiForm
        (infoWidget,bodyWidget) <- case result of 
                FormSuccess ps -> postSantaRSuccess ps
                FormFailure es -> postSantaRFailure es
                FormMissing    -> postSantaRMissing

        defaultLayout $ do
                setTitle "Secret Santa - BrechtSerckx.be"
                $(widgetFile "santa")
        


postSantaRSuccess :: SecretSanta.SantaData -> Handler (Widget,Widget)
postSantaRSuccess santaData = do
        matches <- liftIO $ SecretSanta.randomMatch $ SecretSanta.participants $ santaData
        let info = SecretSanta.santaInfo santaData

        mailSettings <- appMailSettings . appSettings <$> getYesod

        let sendMatchEmail ((participant,email),(match,_))
                = Mail.sendMail mailSettings
                $ mkMail mailSettings email info participant match

        liftIO $ mapM_ sendMatchEmail matches

        let infoWidget = 
                [whamlet|
                Secret Santa generated your matches!
                |]

        let bodyWidget = 
                [whamlet|
                <table .table .table-striped> 
                        <tr>
                                <th .col-md-5 .text-left>Participant
                                <th .col-md-2 .text-center>is Secret Santa for
                                <th .col-md-5 .text-right>Match
                        $forall ((p,_),(m,_)) <- matches
                                <tr>
                                        <td .text-left>#{p}
                                        <td .text-center>-->
                                        <td .text-right>#{m}
                |]
        return (infoWidget,bodyWidget)



postSantaRMissing :: Handler (Widget,Widget)
postSantaRMissing = do
        let infoWidget = 
                [whamlet|
                Error
                |]

        let bodyWidget = 
                [whamlet|
                <p>Error: missing form!
                |]
        return (infoWidget,bodyWidget)


postSantaRFailure :: [Text] -> Handler (Widget,Widget)
postSantaRFailure es = do
        let infoWidget = 
                [whamlet|
                Error
                |]

        let bodyWidget = 
                [whamlet|
                <ul>
                        $forall e <- es
                                <li>#{e}
                |]
        return (infoWidget,bodyWidget)


mkMail :: Mail.MailSettings -> Mail.Address -> SecretSanta.SantaInfo -> Text -> Text -> Mail.Mail
mkMail mailSettings to santaInfo participant match 
        = (Mail.emptyMail $ Mail.mailOrigin mailSettings)
                { Mail.mailTo = [to]
                , Mail.mailHeaders =
                        [ ("Subject", Mail.mailSubject mailSettings)
                        ]
                , Mail.mailParts = [[textPart, htmlPart]]
                }
        where
                textPart = mkTextPart santaInfo participant match
                htmlPart = mkHtmlPart santaInfo participant match

mkTextPart :: SecretSanta.SantaInfo -> Text -> Text -> Mail.Part
mkTextPart (SecretSanta.SantaInfo sDescr sDate sPrice) participant match = Mail.Part
        { Mail.partType = "text/plain; charset=utf-8"
        , Mail.partEncoding = Mail.None
        , Mail.partFilename = Nothing
        , Mail.partContent = encodeUtf8
                [stext|
                        Hi #{participant}
                        You are Secret Santa for: #{match}!

                        $if isJust sDate
                                Date: #{show (fromJust sDate)}
                        $if isJust sPrice
                                Price: #{show (fromJust sPrice)}
                        $if isJust sDescr
                                Description: #{fromJust sDescr}
                |]
        , Mail.partHeaders = []
        }

mkHtmlPart :: SecretSanta.SantaInfo -> Text -> Text -> Mail.Part
mkHtmlPart (SecretSanta.SantaInfo sDescr sDate sPrice) participant match = Mail.Part
        { Mail.partType = "text/html; charset=utf-8"
        , Mail.partEncoding = Mail.None
        , Mail.partFilename = Nothing
        , Mail.partContent = renderHtml
                [shamlet|
                        <p>Hi #{participant}
                        <p>You are Secret Santa for: <b>#{match}<\b>!
                        $maybe date <- sDate 
                                Date: #{show date}
                        $maybe price <- sPrice 
                                Price: #{price}
                        $maybe descr <- sDescr
                                Description: #{descr}
|]
        , Mail.partHeaders = []
        }


