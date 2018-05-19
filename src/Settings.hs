{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import           ClassyPrelude.Yesod
import           qualified Control.Exception as Exception
import           Data.Aeson
    ( Result (..), fromJSON, withObject, (.!=), (.:?)
    )
import           Data.Aeson.Types            (typeMismatch)
import           Data.FileEmbed              (embedFile)
import           Data.Yaml                   (decodeEither')
import           Language.Haskell.TH.Syntax  (Exp, Name, Q)
import           Network.Wai.Handler.Warp    (HostPreference)
import           Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import           Yesod.Default.Util
    ( WidgetFileSettings, widgetFileNoReload, widgetFileReload
    )
import           Network.Mail.SMTP.Auth (UserName, Password)
import           Network.Mail.Mime (Address(..))
import           Mail (MailSettings(..))

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appRoot                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appCopyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code
    
    , appMailSettings           :: MailSettings
    }


instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#ifdef DEVELOPMENT
                    True
#else
                    False
#endif
        appStaticDir              <- o .: "static-dir"
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        dev                       <- o .:? "development"      .!= defaultDev

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
        appShouldLogAll           <- o .:? "should-log-all"   .!= dev
        appReloadTemplates        <- o .:? "reload-templates" .!= dev
        appMutableStatic          <- o .:? "mutable-static"   .!= dev
        appSkipCombining          <- o .:? "skip-combining"   .!= dev

        appCopyright              <- o .: "copyright"
        appAnalytics              <- o .:? "analytics"

        appMailSettings           <- o .: "mail"

        return AppSettings {..}

instance FromJSON MailSettings where
    parseJSON = withObject "MailSettings" $ \o -> do
        mailOriginName  <- o .: "origin_name"
        mailOriginEmail  <- o .: "origin_email"
        let mailOrigin = Address (Just mailOriginName) mailOriginEmail
        mailSubject <- o .: "subject"

        svcType :: Text <- o .: "service"
        case svcType of
            "sendmail" -> return SendMailSettings {..}
            "gmail"    -> do
                gMailUserName :: UserName <- o .: "gmail_username"
                gMailPassword :: Password <- o .: "gmail_password"
                return GMailSettings {..}
            "ses"  -> do
                sesAccessKey        :: Text       <- o .:  "ses_access_key"
                sesSecretKey        :: Text       <- o .:  "ses_secret_key"
                sesSessionToken     :: Maybe Text <- o .:? "ses_session_token"
                sesRegion           :: Text       <- o .:  "ses_region"
                return SesSettings {..}
            _          -> typeMismatch "MailService" $ Object o




-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (
    if appReloadTemplates compileTimeAppSettings
    then widgetFileReload
    else widgetFileNoReload)
     widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue 
    = either Exception.throw id
    $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
