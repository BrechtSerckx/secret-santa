{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyCase #-}
module Handler.Santa where

import Import hiding (count,tail,trace,multiEmailField)
import SecretSanta (randomMatch)
import Data.List (nub,tail)
import Debug.Trace (trace)
import qualified Text.Email.Validate as Email
import qualified Data.ByteString.Char8 as BS
import           Network.Mail.Mime
import Mail      (sendMail,mkMail)

type Participant = (Text,Address)
data SantaData = SantaData {
    participants :: [Participant]
    } deriving (Show,Eq)


multiForm :: Html -> MForm Handler (FormResult SantaData, Widget)
multiForm extra = do
    (namesRes, namesView) <- mreq (multiTextField "Name") "Names" Nothing
    (emailsRes, emailsView) <- mreq (multiEmailField "Email") "Emails" Nothing
    let widget = do
            toWidget
                [lucius||]
            [whamlet|
                #{extra}
                <div .participant_input_wrapper .table .table-striped>
                    <span .form-group .tr .participant_input_proto>
                        ^{fvInput namesView}
                        ^{fvInput emailsView}
                        <span .remove_field .glyphicon .glyphicon-remove .td .col-md-1>
            |]
    let res = case mkSantaData <$> namesRes <*> emailsRes of
            FormSuccess (Right santaData) -> FormSuccess $ trace (show santaData) santaData
            FormSuccess (Left es)         -> FormFailure es
            _                             -> res
    return (res, widget)

mkSantaData :: [Text] -> [Address] -> Either [Text] SantaData
mkSantaData names emails
    | length (nub names) < length names = Left $ return "Your participants must have unique names!"
    | length (nub names) < 2            = Left $ return "You must enter at least two unique participants!"
    | otherwise                         = es
        where
            ps = map mkParticipant 
                    . filter (\(name,email) -> name /= "" || email /= "") 
                    $ zip names emails
            es = case lefts ps of
                []  -> Right $ SantaData $ rights $ ps
                _   -> Left $ lefts ps


mkParticipant :: (Text,Address) -> Either Text Participant
mkParticipant (name,email)
    | name == ""    = Left $ "Name cannot be empty!"
    | email == ""   = Left $ "Email cannot be empty!"
    | otherwise     = Right (name,email)



multiTextField :: Text -> Field Handler [Text]
multiTextField label = Field
    { fieldParse = \rawVals _fileVals -> return $ Right $ Just $ tail rawVals
    , fieldView = \_idAttr nameAttr otherAttrs _eResult _isReq ->
        [whamlet|
                    <span .td .col-md-1>#{label}: 
                    <input name=#{nameAttr} *{otherAttrs} type=text .td .col-md-4>
        |] 
    , fieldEnctype = UrlEncoded
    }


multiEmailField :: Text -> Field Handler [Address]
multiEmailField label = Field
    { fieldParse = \rawVals _fileVals -> return $ parseEmails rawVals 
    , fieldView = \_id name attrs _eResult _isReq ->
        [whamlet|
                    <span .td .col-md-1>#{label}: 
                    <input name=#{name} *{attrs} type=email .td .col-md-4>
        |] 
    , fieldEnctype = UrlEncoded
    }

parseEmails :: [Text] -> Either (SomeMessage (HandlerSite Handler)) (Maybe [Address])
parseEmails es 
    | any (Email.isValid) es' = Right $ Just $ map (\e -> Address Nothing e) $ tail es
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
        <form method=post action=@{SantaR}#forms enctype=#{formEnctype} .form-horizontal>
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
    liftIO $ mapM_ (\((p,e),(m,_)) -> sendMail $ mkMail e p m) matches

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
