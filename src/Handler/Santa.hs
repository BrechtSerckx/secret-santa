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

data ParticipantList = ParticipantList
    { carCustom  :: [Text]
    } deriving Show



santaAForm :: AForm Handler ParticipantList
santaAForm = ParticipantList
    <$> areq santaField "Participants" Nothing


santaForm :: Html -> MForm Handler (FormResult ParticipantList, Widget)
santaForm = renderBootstrap3 bh santaAForm
    where
        bh = BootstrapBasicForm



santaField :: Field Handler [Text]
santaField = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            ps@(_:_:_) -> return $ Right $ Just $ nub . filter (/= "") $ ps
            _          -> return $ Left "You must enter at least two participants"
    , fieldView = \_idAttr nameAttr otherAttrs _eResult _isReq ->
        [whamlet|
            <div .participant_input_wrapper .table .table-striped .table-bordered .table-highlight>
                <span .form-group .tr .participant_input_proto style="display:none">
                    <input name=#{nameAttr} *{otherAttrs} type=text .td>
                    <span .remove_field .glyphicon .glyphicon-remove>
        |] 
    , fieldEnctype = UrlEncoded
    }




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

    let participants = carCustom $ case result of
            FormSuccess res -> res
            FormFailure e -> ParticipantList {carCustom = e}
            FormMissing -> ParticipantList {carCustom = ["missing1","missing2"]}
    matches <- liftIO $ randomMatch participants
        

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Secret Santa - BrechtSerckx.be"
        $(widgetFile "santa-post")
