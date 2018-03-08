{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.MFormTest where
import Import

data Person = Person
    { personName :: Text
    , personAge  :: Int
    }
    deriving Show

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm extra = do
    (nameRes, nameView) <- mreq textField "this is not used" Nothing
    (ageRes, ageView) <- mreq intField "neither is this" Nothing
    let personRes = Person <$> nameRes <*> ageRes
    let widget = do
            toWidget
                [lucius|
                    ##{fvId ageView} {
                        width: 3em;
                    }
                |]
            [whamlet|
                #{extra}
                <p>
                    Hello, my name is #
                    ^{fvInput nameView}
                    \ and I am #
                    ^{fvInput ageView}
                    \ years old. #
                    <input type=submit value="Introduce myself">
            |]
    return (personRes, widget)

getMFormTestR :: Handler Html
getMFormTestR = do
    ((res, widget), enctype) <- runFormGet personForm
    defaultLayout
        [whamlet|
            <p>Result: #{show res}
            <form enctype=#{enctype}>
                ^{widget}
        |]
