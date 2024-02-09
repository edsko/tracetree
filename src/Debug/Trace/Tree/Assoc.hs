{-# LANGUAGE CPP #-}

-- | Association lists
--
-- Intended to be double imported:
--
-- > import Debug.Trace.Tree.Assoc
-- > import qualified Debug.Trace.Tree.Assoc as Assoc
module Debug.Trace.Tree.Assoc (
    Assoc(..)
  , pattern Singleton
  ) where

import Data.Bifunctor
import Data.Functor.Compose

{-------------------------------------------------------------------------------
  Main datatype
-------------------------------------------------------------------------------}

newtype Assoc k v = Assoc { assocList :: [(k, v)] }
  deriving (Show, Eq)

pattern Singleton :: k -> v -> Assoc k v
pattern Singleton k v = Assoc [(k, v)]

{-------------------------------------------------------------------------------
  Standard type class instances
-------------------------------------------------------------------------------}

instance Bifunctor Assoc where
  bimap f g (Assoc vs) = Assoc (map (bimap f g) vs)

instance Functor (Assoc k) where
  fmap = second

instance Foldable (Assoc k) where
  foldMap f = foldMap f . Compose . assocList

instance Traversable (Assoc k) where
  traverse f = fmap (Assoc . getCompose) . traverse f . Compose . assocList

#if MIN_VERSION_base(4,9,0)
instance Semigroup (Assoc k v) where
  (Assoc xs) <> (Assoc ys) = Assoc (xs `mappend` ys)  
#endif

instance Monoid (Assoc k v) where
  mempty = Assoc mempty
#if MIN_VERSION_base(4,9,0)
  mappend = (<>)
#else
  (Assoc xs) `mappend` (Assoc ys) = Assoc (xs `mappend` ys)
#endif
