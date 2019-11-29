-- bunch of language extensions
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Control.Concurrent.STM
import           Data.Aeson
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import           Data.Time
import           GHC.Generics
import           Network.Wai.Handler.Warp
import           Servant

-----------------------------------------------------------------
-- Business logic: a hat that contains users and their matches --
-----------------------------------------------------------------

type Email = T.Text

data User =
  User
  { name :: T.Text
  , email :: T.Text
  } deriving (Eq, Ord, Generic, ToJSON, FromJSON)

type UserMatch = (User,User)
type HatMatch = [UserMatch]

-- | A Hat contains basic info and participants
data Hat (matched :: Bool) where
  -- | UnmatchedHat is a new hat
  UnmatchedHat :: User -> Maybe Day -> Set.Set User -> Hat 'False
  -- | MatchedHat is a hat that also contains matches
  MatchedHat :: User -> Maybe Day -> Set.Set User -> HatMatch -> Hat 'True
type UnmatchedHat = Hat 'False
type MatchedHat = Hat 'True

deriving instance Eq (Hat b)

instance ToJSON (Hat b) where
  toJSON (UnmatchedHat u d us) = object $
    [ "owner" .= u
    , "date" .= d
    , "participants" .= us
    ]
  toJSON (MatchedHat u d us m) = object $
    [ "owner" .= u
    , "date" .= d
    , "participants" .= us
    , "match" .= m
    ]

instance FromJSON UnmatchedHat where
  parseJSON = withObject "UnmatchedHat" $ \v ->
    UnmatchedHat <$> v .: "owner" <*> v .:? "date" <*> v .: "participants"

data AnyHat = forall b. AnyHat (Hat b)

instance Eq AnyHat where
  (AnyHat h@(UnmatchedHat _ _ _)) == (AnyHat h'@(UnmatchedHat _ _ _)) = h == h'
  (AnyHat h@(MatchedHat _ _ _ _)) == (AnyHat h'@(MatchedHat _ _ _ _)) = h == h'

instance ToJSON AnyHat where
  toJSON (AnyHat h) = toJSON h


-----------------------------------------------------------
-- Business logic: match an UnmatchedHat to a MatchedHat --
-----------------------------------------------------------

matchHat :: UnmatchedHat -> MatchedHat
matchHat (UnmatchedHat u d us) = MatchedHat u d us ms
 where
  ms  = zip us' . drop 1 . cycle $ us'
  us' = Set.toList us

-------------------------------------------------------------
-- MonadSanta typeclass/interface against which we program --
-------------------------------------------------------------

type Id = Int

data WithId a =
  WithId
  { id :: Id
  , payload :: a
  }

instance ToJSON a => ToJSON (WithId a) where
  toJSON (WithId id a) = object $
    [ "id" .= id
    , "payload" .= a
    ]

newtype SantaError = SantaError { error :: T.Text }
  deriving (Eq, Generic, ToJSON)

class MonadSanta m where
  -- | Take an unmatched hat and put it in the database
  putHat :: UnmatchedHat -> m Id
  -- | Retrieve a hat from the database
  getHat :: Id -> m (Maybe (WithId AnyHat))
  -- |  Match the hat with the given id
  matchHatById :: Id -> m (Maybe SantaError)

-------------------------
-- Runtime environment --
-------------------------

-- | The memory is simply a map of Ids to Hats
type Memory = Map.Map Id AnyHat

-- | The environment contains a mutable variable to the memory
newtype Env =
  Env
  { envMemory :: TVar Memory
  }

-- | Create an empty environment
emptyEnv :: IO Env
emptyEnv = do
  mem <- newTVarIO Map.empty
  return $ Env mem

---------------------
-- App monad stack --
---------------------

type App = ReaderT Env IO

-------------------------------------------------------------
-- Our instance for MonadSanta that uses the in-memory map --
-------------------------------------------------------------

instance MonadSanta App where

  putHat h = do
    let anyhat = AnyHat h
    tvar <- envMemory <$> ask
    lift . atomically $ stateTVar tvar $ \mem ->
      let n = Map.size mem
      in  (n, Map.insert n anyhat mem)

  getHat n = do
    tvar <- envMemory <$> ask
    mem <- lift $ readTVarIO tvar
    return $ WithId n <$> Map.lookup n mem

  matchHatById n = do
    tvar <- envMemory <$> ask
    mem <- lift $ readTVarIO tvar
    let mhat = Map.lookup n mem
    case mhat of
      Nothing ->
        return . Just . SantaError $ "Hat not found"
      Just (AnyHat (MatchedHat _ _ _ _)) ->
        return . Just . SantaError $ "Hat is already matched"
      Just (AnyHat hat@(UnmatchedHat _ _ _)) -> do
        let hat' = AnyHat . matchHat $ hat
        lift . atomically $ stateTVar tvar $ \mem' ->
          let mhat' = Map.lookup n mem' in
          if mhat /= mhat'
          then (Just . SantaError $ "Hat changed while matching", mem')
          else (Nothing, Map.insert n hat' mem')

----------------------------------------------------
-- Servant API: type-safe API defining our routes --
----------------------------------------------------

type SecretSantaAPI
  = "api" :> "secretsanta" :>
    (  ReqBody '[JSON] UnmatchedHat
    :> Put '[JSON] Id
  :<|> Capture "id" Id
    :> Get '[JSON] (Maybe (WithId AnyHat))
  :<|> Capture "id" Id
    :> "match"
    :> Post '[JSON] (Maybe SantaError)
    )

-- | The function that actually serves the endpoints defined above.
-- brittany-disable-next-binding
secretSantaServer :: Env -> Server SecretSantaAPI
secretSantaServer env = runServer
    $  putHat
  :<|> getHat
  :<|> matchHatById
 where
  runServer = hoistServer (Proxy @SecretSantaAPI) mkHandler
  mkHandler :: App a -> Handler a
  mkHandler a = liftIO $ runReaderT a env

----------
-- MAIN --
----------

-- | Wrapping it all together
main :: IO ()
main = do
  -- print "hello world"
  putStrLn "hello world"
  -- initialize environment
  env <- emptyEnv
  -- run the Servant API server on port 9000
  run 9000 $ serve (Proxy @SecretSantaAPI) (secretSantaServer env)
