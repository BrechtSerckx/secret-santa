{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Text
import Network.Wai.Handler.Warp
import Servant

import SecretSanta.App
import SecretSanta.Hat
import SecretSanta.MonadSanta
import SecretSanta.WithId

main :: IO ()
main = do
  putStrLn "hello world"
  env <- emptyEnv
  run 9000 $ serve (Proxy @SecretSantaAPI) (secretSantaServer env)


type SecretSantaAPI
  = "api" :> "secretsanta" :>
    (  ReqBody '[JSON] UnmatchedHat
    :> Put '[JSON] Id
  :<|> Capture "id" Id
    :> Get '[JSON] (Maybe (HasId AnyHat))
  :<|> Capture "id" Id
    :> "match"
    :> Post '[JSON] (Maybe Text)
    )

secretSantaServer :: Env -> Server SecretSantaAPI
secretSantaServer env = runServer
    $  putHat
  :<|> getHat
  :<|> matchHatById
 where
  runServer = hoistServer (Proxy @SecretSantaAPI) mkHandler
  mkHandler a = liftIO $ runReaderT a env
