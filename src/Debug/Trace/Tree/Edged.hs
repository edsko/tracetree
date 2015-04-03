-- | Edge-labelled rose trees
module Debug.Trace.Tree.Edged (
    ETree(..)
  ) where

import Data.Bifunctor
import Debug.Trace.Tree.Assoc

{-------------------------------------------------------------------------------
  Main datatype
-------------------------------------------------------------------------------}

data ETree k v = ENode v (Assoc k (ETree k v))
  deriving (Show, Eq)

instance Bifunctor ETree where
  bimap f g (ENode v ts) = ENode (g v) (bimap f (bimap f g) ts)

instance Functor (ETree k) where
  fmap = second

instance Foldable (ETree k) where
  foldMap f (ENode v ts) = f v `mappend` foldMap (foldMap f) ts

instance Traversable (ETree k) where
  traverse f (ENode v ts) = ENode <$> f v <*> traverse (traverse f) ts
