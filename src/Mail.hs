{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Mail where

import           Network.Mail.Mime  hiding (htmlPart)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Import
import           Text.Shakespeare.Text (stext)
import qualified Data.Text.Lazy.Encoding

from = Address (Just "Secret Santa" ) "secret-santa@secret-santa.net" 
cc = []
bcc = []
subject = "Your Secret Santa Match"


sendMail :: Mail -> IO ()
sendMail = renderSendMail 

mkMail :: Text -> Text -> Text -> Mail
mkMail to participant match = (emptyMail $ from)
        { mailTo = [Address Nothing to]
        , mailHeaders =
            [ ("Subject", subject)
            ]
        , mailParts = [[textPart, htmlPart]]
        }
    where
        textPart = mkTextPart participant match
        htmlPart = mkHtmlPart participant match

mkTextPart :: Text -> Text -> Part
mkTextPart participant match = Part
    { partType = "text/plain; charset=utf-8"
    , partEncoding = None
    , partFilename = Nothing
    , partContent = Data.Text.Lazy.Encoding.encodeUtf8
        [stext|
            Hi #{participant}
            You are Secret Santa for: #{match}!
        |]
    , partHeaders = []
    }

mkHtmlPart :: Text -> Text -> Part
mkHtmlPart participant match = Part
    { partType = "text/html; charset=utf-8"
    , partEncoding = None
    , partFilename = Nothing
    , partContent = renderHtml
        [shamlet|
            <p>Hi #{participant}
            <p>You are Secret Santa for: <b>#{match}<\b>!
        |]
    , partHeaders = []
    }


