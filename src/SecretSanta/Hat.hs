{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
module SecretSanta.Hat where

import Data.Aeson
import Data.Functor.Identity
import GHC.Generics
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Calendar

type Email = T.Text

data User =
  User
  { name :: T.Text
  , email :: T.Text
  } deriving (Eq, Ord, Generic, ToJSON, FromJSON)

type UserMatch = (User,User)
type HatMatch = [UserMatch]

data Hat (matched :: Bool) where
  UnmatchedHat :: User -> Maybe Day -> Set.Set User -> Hat 'False
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

matchHat :: UnmatchedHat -> MatchedHat
matchHat (UnmatchedHat u d us) = MatchedHat u d us ms
  where
    ms = zip us' . drop 1 . cycle $ us'
    us' = Set.toList us
