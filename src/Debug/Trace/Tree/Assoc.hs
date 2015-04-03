-- | Association lists
module Debug.Trace.Tree.Assoc (
    Assoc(..)
  ) where

import Data.Bifunctor
import Data.Functor.Compose

newtype Assoc k v = Assoc { assocList :: [(k, v)] }
  deriving (Show, Eq)

instance Bifunctor Assoc where
  bimap f g (Assoc vs) = Assoc (map (bimap f g) vs)

instance Functor (Assoc k) where
  fmap = second

instance Foldable (Assoc k) where
  foldMap f = foldMap f . Compose . assocList

instance Traversable (Assoc k) where
  traverse f = fmap (Assoc . getCompose) . traverse f . Compose . assocList
