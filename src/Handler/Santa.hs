{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
module Handler.Santa where


import qualified Data.ByteString.Char8 as BS
import           Data.Either (isLeft,isRight)
import           Data.List (nub)
import           Data.Maybe (fromJust,isJust)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LT
import           Debug.Trace (trace)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html.Renderer.Text as BT (renderHtml)
import qualified Text.Email.Validate as Email
import           Text.Hamlet (shamlet)
import           Text.Pandoc.Class (runPure)
import           Text.Pandoc.Readers (readHtml)
import           Text.Pandoc.Writers (writePlain)

import           Import hiding (count,tail,trace,id,encodeUtf8,multiEmailField)
import qualified Mail (sendMail,MailSettings(..),Address(..),Part(..),Mail(..),emptyMail,Encoding(..))
import qualified SecretSanta
import qualified SecretSanta.Match as SS.Match



multiForm :: Html -> MForm Handler (FormResult SecretSanta.SantaData, Widget)
multiForm extra = do
        -- description
        let descrFieldSettings = FieldSettings "Description" (Just "Enter a description") (Just "description") (Just "description") [("rows","6"),("class","form-control")]
        (descrRes, descrView) <- mopt textareaField descrFieldSettings Nothing
        -- date
        let dateFieldSettings = FieldSettings "Date" (Just "Enter a date") (Just "date") (Just "date") [("class","form-control ")]
        date <- utctDay <$> liftIO getCurrentTime
        (dateRes, dateView) <- mopt dayField dateFieldSettings $ Just $ Just $ date
        -- price
        let priceFieldSettings = FieldSettings "Price" (Just "Enter a price") (Just "price") (Just "price") [("class","form-control "),("step","0.01"),("min","0")]
        (priceRes, priceView) <- mopt doubleField priceFieldSettings $ Just $ Just 5
        -- names
        let namesFieldSettings = FieldSettings "Name" (Just "Enter a names") (Just "names") (Just "names") [("class","form-control "),("data-unique","")]
        (namesRes, namesView) <- mreq multiTextField namesFieldSettings Nothing
        -- email
        let emailsFieldSettings = FieldSettings "Email" (Just "Enter a emails") (Just "emails") (Just "emails") [("class","form-control ")]
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
        { fieldParse = \rawVals _fileVals -> return $ Right $ Just rawVals
        , fieldView = \theId name attrs val isReq ->
                [whamlet|
                        <input id="#{theId}" name="#{name}" *{attrs} type=text :isReq:required :(isLeft val):data-defaults="" :(isRight val):data-defaults="#{either id (intercalate ",") val}">
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
                        <input id="#{theId}" name="#{name}" *{attrs} type=email :isReq:required :(isLeft val):data-defaults="" :(isRight val):data-defaults="#{either id (intercalate ",") val}">
                |]
        , fieldEnctype = UrlEncoded
        }

parseEmails :: [Text] -> Either (SomeMessage (HandlerSite Handler)) (Maybe [Text])
parseEmails es 
        | any (Email.isValid) es' = Right $ Just es
        | otherwise               = Left $ "Invalid email address!"
                where
                        es' :: [ByteString]
                        es' = map (BS.pack . unpack) es


createForm (formWidget,formEnctype) = [whamlet|
                <form method=post action=@{SantaR}#forms enctype=#{formEnctype} role="form" data-toggle="validator" .form>
                        ^{formWidget}
                        <div .row>
                                <div .col-lg-5 .col-md-5 .col-sm-4 .col-xs-6>
                                        <button type=button .add_field_button .btn .btn-info .btn-lg .btn-block >
                                                <span .pull-left .glyphicon .glyphicon-plus>
                                                Add More
                                <div .col-lg-7 .col-md-7 .col-sm-8 .col-xs-6>
                                        <button .btn.btn-primary type="submit" .btn-lg .btn-block>
                                                <span .pull-left .glyphicon .glyphicon-random>
                                                Match!
                |]

getSantaR :: Handler Html
getSantaR = do
        form <- generateFormPost multiForm
        let infoWidget = [whamlet|
                Please fill in the participant names.
                |]
        let bodyWidget = createForm form

        defaultLayout $ do
                setTitle "Secret Santa - BrechtSerckx.be"
                $(widgetFile "santa")


postSantaR :: Handler Html
postSantaR = do
        ((result, fw), enc) <- runFormPost multiForm
        let form = (fw,enc)
        case result of
                FormSuccess res -> sendMails res >> return ()
                _               -> return ()
        let infoWidget = case result of 
                FormSuccess _ -> [whamlet|Secret Santa successfully submitted!|]
                _             -> [whamlet|Please fill in the participant names.|]
        let formWidget = createForm form
        let bodyWidget = case result of 
                FormSuccess _ -> [whamlet|Secret-Santa.xyz will now notify all participants!|]
                FormFailure es -> [whamlet|
                        <div .alert .alert-danger>
                                There are errors in the form:
                                <ul>
                                        $forall e <- es
                                                <li>#{e}
                        ^{formWidget}
                        |]
                FormMissing    -> [whamlet|
                        <div .alert .alert-warning>
                                Please fill in the participants.
                        ^{formWidget}
                        |]

        defaultLayout $ do
                setTitle "Secret Santa - BrechtSerckx.be"
                $(widgetFile "santa")
        
sendMails :: SecretSanta.SantaData -> Handler [(SecretSanta.Participant,SecretSanta.Participant)]
sendMails santaData = do
        matches <- liftIO $ SS.Match.randomMatch $ SecretSanta.participants $ santaData
        let info = SecretSanta.santaInfo santaData

        mailSettings <- appMailSettings . appSettings <$> getYesod

        let sendMatchEmail ((participant,email),(match,_))
                = Mail.sendMail mailSettings
                $ mkMail mailSettings email info participant match

        liftIO $ mapM_ sendMatchEmail matches
        return matches

postSantaRSuccess :: SecretSanta.SantaData -> Handler (Widget,Widget)
postSantaRSuccess santaData = do
        matches <- sendMails santaData
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
                mailContent = mkMailContent santaInfo participant match
                textPart = mkTextPart mailContent
                htmlPart = mkHtmlPart mailContent

mkTextPart :: Html -> Mail.Part
mkTextPart html = Mail.Part
        { Mail.partType = "text/plain; charset=utf-8"
        , Mail.partEncoding = Mail.None
        , Mail.partFilename = Nothing
        , Mail.partContent = encodeUtf8 . LT.fromStrict $ html2Text html
        , Mail.partHeaders = []
        }


html2Text :: Html -> Text
html2Text html = 
        let
                rendered = LT.toStrict . BT.renderHtml $ html
                converted = do
                        doc <- readHtml def rendered
                        writePlain def doc
        in case runPure converted of
                Left e  -> pack $ show e
                Right t -> t



mkHtmlPart :: Html -> Mail.Part
mkHtmlPart html = Mail.Part
        { Mail.partType = "text/html; charset=utf-8"
        , Mail.partEncoding = Mail.None
        , Mail.partFilename = Nothing
        , Mail.partContent = renderHtml html
        , Mail.partHeaders = []
        }

mkMailContent :: SecretSanta.SantaInfo -> Text -> Text -> Html
mkMailContent (SecretSanta.SantaInfo sDescr sDate sPrice) participant match =
        [shamlet|
                <p>Hi #{participant}
                <p>You are Secret Santa for: <b>#{match}</b>!
                <p>Additional info:
                        <ul>
                                $maybe date <- sDate 
                                        <li>Date: #{show date}
                                $maybe price <- sPrice 
                                        <li>Price: #{price}
                                $maybe descr <- sDescr
                                        <li>Description: #{descr}
        |]

