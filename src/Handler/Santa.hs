{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Santa where

import Import hiding (count,tail)
import Yesod.Form.Bootstrap3
import SecretSanta (randomMatch)
import Data.List (nub,tail)



type ParticipantList = [Text]


multiForm :: Html -> MForm Handler (FormResult (ParticipantList,Int), Widget)
multiForm html = do
    let countFieldSettings = FieldSettings { fsLabel = ""
                                           , fsTooltip = Nothing
                                           , fsId = Nothing
                                           , fsName = Nothing
                                           , fsAttrs = [("class", "count_input")]
                                           }
    (res, widget) <- flip (renderBootstrap3 BootstrapBasicForm) html $ (,)
            <$> areq santaField "Santa" Nothing
            <*> areq intField countFieldSettings (Just 2)
    return $ case res of
              FormSuccess (ps, count)
                        | length (nub ps) /= count ->
                      let msg = "Invalid participant count"
                       in (FormFailure [msg], [whamlet|
                  <p .errors>#{msg}
                  ^{widget}
                  |])
              _ -> (res, widget)


santaField :: Field Handler [Text]
santaField = Field
    { fieldParse = \rawVals _fileVals -> return $ validateSantaField $ tail rawVals
    , fieldView = \_idAttr nameAttr otherAttrs _eResult _isReq ->
        [whamlet|
            <div .participant_input_wrapper .table .table-striped>
                <span .form-group .tr .participant_input_proto>
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
    | otherwise                     = Right $ Just ps'
        where ps' = filter (/= "") ps
                            


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
    let infoWidget = [whamlet|
        Secret Santa generated your matches!
        |]

    let bodyWidget = case result of
            FormSuccess (participants,_) -> do
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
        $(widgetFile "santa")
