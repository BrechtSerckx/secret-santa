{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Santa where

import Import


data Car = Car
    { carModel :: Text
    , carYear  :: Int
    } deriving Show

carAForm :: AForm Handler Car
carAForm = Car
    <$> areq textField "Model" (Just "tesla")
    <*> areq intField "Year" (Just 2015)

carForm :: Html -> MForm Handler (FormResult Car, Widget)
carForm = renderTable carAForm


getSantaR :: Handler Html
getSantaR = do
    (formWidget, formEnctype) <- generateFormPost carForm
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Secret Santa - BrechtSerckx.be"
        $(widgetFile "santa")


postSantaR :: Handler Html
postSantaR = do
    ((result, formWidget), formEnctype) <- runFormPost carForm
    --let submission = case result of
    --        FormSuccess res -> Just res
    --        _ -> Nothing

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "santa")
