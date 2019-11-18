{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module SecretSanta.App where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Trans.Class
import Control.Monad.Reader
import qualified Data.Map as Map

import SecretSanta.Hat
import SecretSanta.MonadSanta
import SecretSanta.WithId

type Memory = Map.Map Id AnyHat

newtype Env =
  Env
  { envMemory :: TVar Memory
  }

emptyEnv :: IO Env
emptyEnv = do
  mem <- newTVarIO Map.empty
  return $ Env mem

type App = ReaderT Env IO

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
    return $ HasId n <$> Map.lookup n mem

  matchHatById n = do
    tvar <- envMemory <$> ask
    mem <- lift $ readTVarIO tvar
    let mhat = Map.lookup n mem
    case mhat of
      Nothing ->
        return . Just $ "Hat not found"
      Just (AnyHat (MatchedHat _ _ _ _)) ->
        return . Just $ "Hat is already matched"
      Just (AnyHat hat@(UnmatchedHat _ _ _)) -> do
        let hat' = AnyHat . matchHat $ hat
        lift . atomically $ stateTVar tvar $ \mem' ->
          let mhat' = Map.lookup n mem' in
          if mhat /= mhat'
          then (Just $ "Hat changed while matching", mem')
          else (Nothing, Map.insert n hat' mem')

