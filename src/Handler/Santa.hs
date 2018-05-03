{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyCase #-}
module Handler.Santa where

import Import hiding (count,tail,trace,multiEmailField,id)
import SecretSanta (randomMatch)
import Data.List (nub,tail)
import Debug.Trace (trace)
import qualified Text.Email.Validate as Email
import qualified Data.ByteString.Char8 as BS
import           Network.Mail.Mime
import Mail      (sendMail,mkMail)
import Data.Either (isLeft)

type Participant = (Text,Address)

data SantaInfo = SantaInfo
    { santaDescr :: Text
    , santaDate  :: Day
    , santaPrice :: Double
    } deriving (Show,Eq)
    
    

data SantaData = SantaData
    { santaInfo    :: SantaInfo
    , participants :: [Participant]
    } deriving (Show,Eq)


multiForm :: Html -> MForm Handler (FormResult SantaData, Widget)
multiForm extra = do
    let descrFieldSettings = FieldSettings "Description" (Just "Enter a description") (Just "description") (Just "description") [("class","col-xs-12 col-md-9")]
    (descrRes, descrView) <- mreq textareaField descrFieldSettings $ Just $ Textarea "Enter a description here"
    let dateFieldSettings = FieldSettings "Date" (Just "Enter a date") (Just "date") (Just "date") [("class","col-xs-12 col-md-9")]
    (dateRes, dateView) <- mreq dayField dateFieldSettings Nothing
    let priceFieldSettings = FieldSettings "Price" (Just "Enter a price") (Just "price") (Just "price") [("class","col-xs-12 col-md-9")]
    (priceRes, priceView) <- mreq doubleField priceFieldSettings $ Just 5
    let namesFieldSettings = FieldSettings "Name" (Just "Enter a names") (Just "names") (Just "names") [("class","col-xs-12 col-sm-10 col-md-3")]
    (namesRes, namesView) <- mreq multiTextField namesFieldSettings Nothing
    let emailsFieldSettings = FieldSettings "Email" (Just "Enter a emails") (Just "emails") (Just "emails") [("class","col-xs-12 col-sm-10 col-md-4")]
    (emailsRes, emailsView) <- mreq multiEmailField emailsFieldSettings Nothing
    let widget = do
            toWidget
                [lucius||]
            [whamlet|
                #{extra}
                <div .row>
                    <h4>
                        General

                <div .row >
                    <label .col-xs-12 .hidden-md .hidden-lg for="description" >
                        ^{fvLabel descrView}
                    <div .form-group>
                        <label .col-xs-2 .hidden-xs .hidden-sm for="description" >
                            ^{fvLabel descrView}
                        ^{fvInput descrView}
                        <div .col-xs-1>
                <br>

                <div .row >
                    <label .col-xs-12 .hidden-md .hidden-lg for="date" >
                        ^{fvLabel dateView}
                    <div .form-group>
                        <label .col-xs-2 .hidden-xs .hidden-sm for="date" >
                            ^{fvLabel dateView}
                        ^{fvInput dateView}
                        <div .col-xs-1>
                <br>

                <div .row >
                    <label .col-xs-12 .hidden-md .hidden-lg for="price" >
                        ^{fvLabel priceView}
                    <div .form-group>
                        <label .col-xs-2 .hidden-xs .hidden-sm for="price" >
                            ^{fvLabel priceView}
                        ^{fvInput priceView}
                        <div .col-xs-1>
                <br>

                <div .row>
                    <h4>
                        Participants:
                
                <div .row .participant_input_wrapper >
                    <div .form-group .participant_input_proto>
                        <div>
                            <label .col-sm-2 .col-md-2 .hidden-xs>
                                ^{fvLabel namesView}
                            ^{fvInput namesView}
                        <div>
                            <label .col-sm-2 .col-md-2 .hidden-xs>
                                ^{fvLabel emailsView}
                            ^{fvInput emailsView}
                        <div .remove_field .glyphicon .glyphicon-remove .col-xs-12 .col-md-1>
                        <br>
            |]
    let infoRes = mkSantaInfo <$> descrRes <*> dateRes <*> priceRes 
    let psRes = mkSantaParticipants <$> namesRes <*> emailsRes
    let res = case mkSantaData <$> infoRes <*> psRes of
            FormSuccess (Right santaData) -> FormSuccess $ trace (show santaData) santaData
            FormSuccess (Left es)         -> FormFailure es
            _                             -> res
    return (res, widget)


mkSantaData :: Either [Text] SantaInfo -> Either [Text] [Participant] -> Either [Text] SantaData
mkSantaData (Left es) _         = Left es
mkSantaData _         (Left es) = Left es
mkSantaData (Right info) (Right ps) = Right $ SantaData info ps


mkSantaInfo :: Textarea -> Day -> Double -> Either [Text] SantaInfo
mkSantaInfo descr day price
    | price < 0 = Left $ return "Price must be positive!"
    | otherwise = Right $ SantaInfo (unTextarea descr) day price
    

mkSantaParticipants :: [Text] -> [Text] -> Either [Text] [Participant]
mkSantaParticipants names emails
    | length (nub names) < length names = Left $ return "Your participants must have unique names!"
    | length (nub names) < 2            = Left $ return "You must enter at least two unique participants!"
    | otherwise                         = es
        where
            ps = map mkParticipant 
                    . filter (\(name,email) -> name /= "" || email /= "") 
                    $ zip names emails
            es 
                | any (isLeft) ps = Left $ lefts ps
                | otherwise       = Right $ rights $ ps


mkParticipant :: (Text,Text) -> Either Text Participant
mkParticipant (name,email)
    | name == ""    = Left $ "Name cannot be empty!"
    | email == ""   = Left $ "Email cannot be empty!"
    | otherwise     = Right (name,Address (Just name) email)



multiTextField :: Field Handler [Text]
multiTextField = Field
    { fieldParse = \rawVals _fileVals -> return $ Right $ Just $ tail rawVals
    , fieldView = \theId name attrs val isReq ->
        [whamlet|
            <input id="#{theId}" name="#{name}" *{attrs} type=text :isReq:required>
        |] 
        {--[whamlet|
            <input id="#{theId}" name="#{name}" *{attrs} type=text :isReq:required value="#{either id id val}">
        |]--}
    , fieldEnctype = UrlEncoded
    }


multiEmailField :: Field Handler [Text]
multiEmailField = Field
    { fieldParse = \rawVals _fileVals -> return $ parseEmails rawVals 
    , fieldView = \theId name attrs val isReq ->
        [whamlet|
            <input id="#{theId}" name="#{name}" *{attrs} type=email :isReq:required>
        |]
        {--[whamlet|
            <input id="#{theId}" name="#{name}" *{attrs} type=email :isReq:required value="#{either id id val}">
        |]--}
    , fieldEnctype = UrlEncoded
    }

parseEmails :: [Text] -> Either (SomeMessage (HandlerSite Handler)) (Maybe [Text])
parseEmails es 
    | any (Email.isValid) es' = Right $ Just $ tail es
    | otherwise               = Left $ "Invalid email address!"
        where
            es' :: [ByteString]
            es' = map (BS.pack . unpack) es
{--
emailField :: RenderMessage master FormMessage => Field sub master Text
emailField = Field
    { fieldParse = blank $
        \s -> if Email.isValid (unpack s)
                then Right s
                else Left $ MsgInvalidEmail s
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
<input id="#{theId}" name="#{name}" *{attrs} type="email" :isReq:required="" value="#{either id id val}">
|]

--}

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
        aDomId <- newIdent
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
        aDomId <- newIdent
        setTitle "Secret Santa - BrechtSerckx.be"
        $(widgetFile "santa")
    


postSantaRSuccess :: SantaData -> Handler (Widget,Widget)
postSantaRSuccess santaData = do
    matches <- liftIO $ randomMatch $ participants $ santaData
    yesod <- getYesod
    let mailService = appMailService yesod
    liftIO $ mapM_ (\((p,e),(m,_)) -> sendMail mailService $ mkMail e p m) matches

    let infoWidget = [whamlet|
            Secret Santa generated your matches!
            |]

    let bodyWidget = [whamlet|
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
    let infoWidget = [whamlet|
                Error
                |]

    let bodyWidget = [whamlet|
                <p>Error: missing form!
                |]
    return (infoWidget,bodyWidget)


postSantaRFailure :: [Text] -> Handler (Widget,Widget)
postSantaRFailure es = do
    let infoWidget = [whamlet|
                Error
                |]

    let bodyWidget = [whamlet|
                <ul>
                    $forall e <- es
                        <li>#{e}
                |]
    return (infoWidget,bodyWidget)
