{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyCase #-}
module Handler.Santa where

import Import hiding (count,tail,trace)
import SecretSanta (randomMatch)
import Data.List (nub,tail)
import Debug.Trace (trace)

type Participant = (Text,Text)
data SantaData = SantaData {
    participants :: [Participant]
    } deriving (Show,Eq)


multiForm :: Html -> MForm Handler (FormResult SantaData, Widget)
multiForm extra = do
    (namesRes, namesView) <- mreq (multiField "Name") "Names" Nothing
    (emailsRes, emailsView) <- mreq (multiField "Email") "Emails" Nothing
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



multiField :: Text -> Field Handler [Text]
multiField label = Field
    { fieldParse = \rawVals _fileVals -> return $ Right $ Just $ tail rawVals
    , fieldView = \_idAttr nameAttr otherAttrs _eResult _isReq ->
        [whamlet|
                    <span .td .col-md-1>#{label}: 
                    <input name=#{nameAttr} *{otherAttrs} type=text .td .col-md-4>
        |] 
    , fieldEnctype = UrlEncoded
    }


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
