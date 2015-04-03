-- | Edge-labelled rose trees
--
-- Intended to be double imported:
--
-- > import Debug.Trace.Tree.Edged
-- > import qualified Debug.Trace.Tree.Edged as Edged
module Debug.Trace.Tree.Edged (
    ETree(..)
  ) where

import Data.Bifunctor
import Debug.Trace.Tree.Assoc

{-------------------------------------------------------------------------------
  Main datatype
-------------------------------------------------------------------------------}

data ETree k v = Node v (Assoc k (ETree k v))
  deriving (Show, Eq)

{-------------------------------------------------------------------------------
  Standard type class instances
-------------------------------------------------------------------------------}

instance Bifunctor ETree where
  bimap f g (Node v ts) = Node (g v) (bimap f (bimap f g) ts)

instance Functor (ETree k) where
  fmap = second

instance Foldable (ETree k) where
  foldMap f (Node v ts) = f v `mappend` foldMap (foldMap f) ts

instance Traversable (ETree k) where
  traverse f (Node v ts) = Node <$> f v <*> traverse (traverse f) ts
