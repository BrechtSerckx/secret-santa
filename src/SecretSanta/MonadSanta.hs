module SecretSanta.MonadSanta where

import Data.Text

import SecretSanta.Hat
import SecretSanta.WithId

class MonadSanta m where
  -- | Take an unmatched hat and put it in the database
  putHat :: UnmatchedHat -> m Id
  -- | Retrieve a hat from the database
  getHat :: Id -> m (Maybe (HasId AnyHat))
  -- |  Match the hat with the given id
  matchHatById :: Id -> m (Maybe Text)
