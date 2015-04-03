-- | Generic translation to SimpleTrees using GHC.Generics
module Debug.Trace.Tree.Generic (
    GSimpleTree(..)
  ) where

import Control.Monad.State
import GHC.Generics
import Debug.Trace.Tree.Assoc
import Debug.Trace.Tree.Simple

{-------------------------------------------------------------------------------
  Translation from arbitrary datatypes to SimpleTree using GHC.Generics
-------------------------------------------------------------------------------}

class GSimpleTree a where
  fromGeneric :: a -> SimpleTree
  default fromGeneric :: (Generic a, GToTree (Rep a)) => a -> SimpleTree
  fromGeneric = gtoTree . from

instance GSimpleTree ()
instance GSimpleTree Bool
instance (GSimpleTree a, GSimpleTree b) => GSimpleTree (a, b)
instance (GSimpleTree a, GSimpleTree b, GSimpleTree c) => GSimpleTree (a, b, c)

instance GSimpleTree Char where fromGeneric = Leaf . show
instance GSimpleTree Int  where fromGeneric = Leaf . show

{-------------------------------------------------------------------------------
  Top-level generic translation
-------------------------------------------------------------------------------}

class GToTree f where
  gtoTree :: f a -> SimpleTree

instance GToTree f => GToTree (M1 D d f) where
  gtoTree (M1 x) = gtoTree x

instance (Constructor c, GToTrees f) => GToTree (M1 C c f) where
  gtoTree c@(M1 x) = Node (conName c) (evalState (gtoTrees x) 0)

instance (GToTree f, GToTree g) => GToTree (f :+: g) where
  gtoTree (L1 x) = gtoTree x
  gtoTree (R1 x) = gtoTree x

instance GSimpleTree a => GToTree (K1 R a) where
  gtoTree (K1 x) = fromGeneric x

{-------------------------------------------------------------------------------
  Generic translation for constructor arguments

  The state is used to assign names to unnamed constructor arguments.
-------------------------------------------------------------------------------}

class GToTrees f where
  gtoTrees :: f a -> State Int (Assoc String SimpleTree)

instance GToTrees U1 where
  gtoTrees U1 = return $ Assoc []

instance (GToTrees f, GToTrees g) => GToTrees (f :*: g) where
  gtoTrees (x :*: y) = mappend <$> gtoTrees x <*> gtoTrees y

instance (Selector s, GToTree f) => GToTrees (M1 S s f) where
  gtoTrees s@(M1 x) = Singleton <$> mkName (selName s) <*> pure (gtoTree x)
    where
      mkName :: String -> State Int String
      mkName "" = state $ \i -> (show i, i + 1)
      mkName nm = return nm
