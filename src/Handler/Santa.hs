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

type Participant = (Text,Text)
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

mkSantaData :: [Text] -> [Text] -> Either [Text] SantaData
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


mkParticipant :: (Text,Text) -> Either Text Participant
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


multiEmailField :: Text -> Field Handler [Text]
multiEmailField label = Field
    { fieldParse = \rawVals _fileVals -> return $ parseEmails rawVals 
    , fieldView = \_id name attrs _eResult _isReq ->
        [whamlet|
                    <span .td .col-md-1>#{label}: 
                    <input name=#{name} *{attrs} type=email .td .col-md-4>
        |] 
    , fieldEnctype = UrlEncoded
    }

parseEmails :: [Text] -> Either (SomeMessage (HandlerSite Handler)) (Maybe [Text])
parseEmails es 
    | any (Email.isValid) es' = Right $ Just $ tail es
    | otherwise               = Left "Invalid Email"
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
    let infoWidget = case result of 
            FormSuccess _ -> [whamlet|
                Secret Santa generated your matches!
                |]
            _             -> [whamlet|
                Error
                |]

    let bodyWidget = case result of
            FormSuccess ps -> do
                matches <- liftIO $ randomMatch $ participants $ ps
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
            FormFailure es -> [whamlet|
                <ul>
                    $forall e <- es
                        <li>#{e}
                |]
            FormMissing -> [whamlet|
                <p>Error: missing form!
                |]

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Secret Santa - BrechtSerckx.be"
        $(widgetFile "santa")
