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
    :> Put '[JSON] AppId
  :<|> Capture "id" AppId
    :> Get '[JSON] (Maybe (HasId App AnyHat))
  :<|> Capture "id" AppId
    :> "match"
    :> Post '[JSON] (Maybe SantaError)
    )

secretSantaServer :: Env -> Server SecretSantaAPI
secretSantaServer env = runServer
    $  putHat
  :<|> getHat
  :<|> matchHatById
 where
  runServer = hoistServer (Proxy @SecretSantaAPI) mkHandler
  mkHandler a = liftIO $ runReaderT a env
