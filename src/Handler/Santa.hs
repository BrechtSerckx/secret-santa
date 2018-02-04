{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Santa where

import Import
import Yesod.Form.Bootstrap3
import SecretSanta (randomMatch)
import Data.List (nub)

type ParticipantList = [Text]



santaAForm :: AForm Handler ParticipantList
santaAForm = areq santaField "Participants" Nothing


santaForm :: Html -> MForm Handler (FormResult ParticipantList, Widget)
santaForm = renderBootstrap3 bh santaAForm
    where
        bh = BootstrapBasicForm



santaField :: Field Handler [Text]
santaField = Field
    { fieldParse = \rawVals _fileVals -> return $ validateSantaField rawVals
    , fieldView = \_idAttr nameAttr otherAttrs _eResult _isReq ->
        [whamlet|
            <div .participant_input_wrapper .table .table-striped>
                <span .form-group .tr .participant_input_proto style="display:none">
                    <span .td .col-md-2>Name: 
                    <input name=#{nameAttr} *{otherAttrs} type=text .td .col-md-9>
                    <span .remove_field .glyphicon .glyphicon-remove .td .col-md-1>
        |] 
    , fieldEnctype = UrlEncoded
    }


validateSantaField :: [Text] -> Either (SomeMessage (HandlerSite Handler)) (Maybe [Text])
validateSantaField ps 
    | length (nub ps') < 2          = Left $ "You must enter at least two unique participants!"
    | length (nub ps') < length ps' = Left $ "Your participants must have unique names!"
    | otherwise                     = Right $ Just ps
        where ps' = filter (/= "") ps
                            



getSantaR :: Handler Html
getSantaR = do
    (formWidget, formEnctype) <- generateFormPost santaForm
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Secret Santa - BrechtSerckx.be"
        $(widgetFile "santa-get")

postSantaR :: Handler Html
postSantaR = do
    ((result, _formWidget), _formEnctype) <- runFormPost santaForm
    let matchWidget = case result of
            FormSuccess participants -> do
                matches <- liftIO $ randomMatch participants
                [whamlet|
                    <table .table .table-striped> 
                        <tr>
                            <th .col-md-5 .text-left>Participant
                            <th .col-md-2 .text-center>is Secret Santa for
                            <th .col-md-5 .text-right>Match
                        $forall (p,m) <- matches
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
        $(widgetFile "santa-post")

