{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.MassInputFormTest where
import Import
import Yesod.Form.MassInput


fixType :: Handler a -> Handler a
fixType = id


myMassForm :: Html -> MForm Handler (FormResult [(Text,Text)], Widget)
myMassForm = renderTable $ inputList "People" massTable
    (\x -> (,)
        <$> areq textField "Name" (fmap fst x)
        <*> areq emailField "Email" (fmap snd x)) Nothing

getMassInputFormTestR :: Handler Html
getMassInputFormTestR = do
    ((res, form), enctype) <- fixType $ runFormGet $ myMassForm
    defaultLayout [whamlet|
<p>Result: #{show res}
<form enctype=#{enctype}>
    <table>
        ^{form}
    <div>
        <input type=submit>
|]

