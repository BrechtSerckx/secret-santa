{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.Map as Map
import Data.SecretSanta.Hat
import Data.Text
import Data.WithId
import Network.Wai
import Network.Wai.Handler.Warp
import SecretSanta.MonadSanta
import SecretSanta.MonadSanta.InMemory
import Servant

main :: IO ()
main = do
  putStrLn "hello world"
  mem <- newTVarIO Map.empty
  run 9000 $ serve (Proxy @SecretSantaAPI) (secretSantaServer mem)

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

inMemorySantaToHandler :: (TVar Memory) -> InMemorySanta a -> Handler a
inMemorySantaToHandler mem r = liftIO $ runReaderT r mem

secretSantaServer :: TVar Memory -> Server SecretSantaAPI
secretSantaServer mem = hoistServer (Proxy @SecretSantaAPI) (inMemorySantaToHandler mem)
    $  putHat
  :<|> getHat
  :<|> matchHatById
