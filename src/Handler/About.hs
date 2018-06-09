{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.About where

import           Import

getAboutR :: Handler Html
getAboutR = do
        defaultLayout $ do
                aDomId <- newIdent
                setTitle "Secret Santa - About"
                $(widgetFile "about")
