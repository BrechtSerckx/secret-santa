{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Santa where

import Import
import Yesod.Form.Bootstrap3

data ParticipantList = ParticipantList
    { carCustom  :: [Text]
    } deriving Show


santaAForm :: AForm Handler ParticipantList
santaAForm = ParticipantList
    <$> areq santaField "Participants" Nothing


santaForm :: Html -> MForm Handler (FormResult ParticipantList, Widget)
santaForm = renderBootstrap3 bh santaAForm
    where
        bh = BootstrapHorizontalForm
            { bflLabelOffset = ColLg 0
            , bflLabelSize = ColLg 1
            , bflInputOffset = ColSm 1
            , bflInputSize = ColLg 1
            }


santaField :: Field Handler [Text]
santaField = Field
    { fieldParse = \rawVals _fileVals ->
        return $ validateSantaField rawVals
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
            <input id=test name=#{nameAttr} *{otherAttrs} type=text>
            <input id=test name=#{nameAttr} *{otherAttrs} type=text>
            <input id=test name=#{nameAttr} *{otherAttrs} type=text>
        |]
    , fieldEnctype = UrlEncoded
    }

validateSantaField :: [Text] -> Either (SomeMessage (HandlerSite Handler)) (Maybe [Text])
validateSantaField ps@(_:_) = Right $ Just ps
validateSantaField [] = Left "invalid"


getSantaR :: Handler Html
getSantaR = do
    (formWidget, formEnctype) <- generateFormPost santaForm
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Secret Santa - BrechtSerckx.be"
        $(widgetFile "santa")

postSantaR :: Handler Html
postSantaR = do
    ((_, formWidget), formEnctype) <- runFormPost santaForm
    --let submission = case result of
    --        FormSuccess res -> Just res
    --        _ -> Nothing

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "santa")
