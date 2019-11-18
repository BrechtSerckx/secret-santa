{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.WithId where

import Data.Aeson


type Id = Int

data WithId a (hasId :: Bool) where
  HasId :: Id -> a -> WithId a 'True
  NoId :: a -> WithId a 'False
type HasId a = WithId a 'True
type NoId a = WithId a 'False


instance ToJSON a => ToJSON (WithId a b) where
  toJSON (HasId id a) = object $
    [ "id" .= id
    , "payload" .= a
    ]
  toJSON (NoId a) = toJSON a
