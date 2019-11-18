{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module SecretSanta.MonadSanta where

import Data.Aeson
import Data.Text
import GHC.Generics

import SecretSanta.Hat

newtype SantaError = SantaError { error :: Text }
  deriving (Eq, Generic, ToJSON)

class MonadSanta m where
  type Id m :: *

  -- | Take an unmatched hat and put it in the database
  putHat :: UnmatchedHat -> m (Id m)
  -- | Retrieve a hat from the database
  getHat :: Id m -> m (Maybe (HasId m AnyHat))
  -- |  Match the hat with the given id
  matchHatById :: Id m -> m (Maybe SantaError)


data WithId m a (hasId :: Bool) where
  HasId :: (Id m) -> a -> WithId m a 'True
  NoId :: a -> WithId m a 'False
type HasId m a = WithId m a 'True
type NoId m a = WithId m a 'False


instance (ToJSON (Id m),ToJSON a) => ToJSON (WithId m a b) where
  toJSON (HasId id a) = object $
    [ "id" .= id
    , "payload" .= a
    ]
  toJSON (NoId a) = toJSON a
