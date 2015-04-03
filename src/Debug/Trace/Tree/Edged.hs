-- | Edge-labelled rose trees
--
-- Intended to be double imported:
--
-- > import Debug.Trace.Tree.Edged
-- > import qualified Debug.Trace.Tree.Edged as Edged
module Debug.Trace.Tree.Edged (
    ETree(..)
    -- * Standard operations
  , elems
  , keys
  , mapEdges
    -- * Additional operations
  , limitBreath
  , numberLevels
  , markFirstChild
    -- * Interaction between ETree and Tree
  , pushEdges
  , pullEdges
  , liftTree
  , liftTree'
  ) where

import Data.Bifunctor
import Data.Foldable
import Data.Tree (Tree)
import qualified Data.Tree as Tree

import Debug.Trace.Tree.Assoc
import qualified Debug.Trace.Tree.Rose as Tree

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

{-------------------------------------------------------------------------------
  Standard operations
-------------------------------------------------------------------------------}

elems :: ETree k v -> [v]
elems = toList

keys :: ETree k v -> [k]
keys (Node _ (Assoc ts)) = concatMap aux ts
  where
    aux :: (k, ETree k v) -> [k]
    aux (k, t) = k : keys t

-- | Change the edges of the tree, providing source and target
mapEdges :: (v -> v -> k -> k') -> ETree k v -> ETree k' v
mapEdges f (Node v (Assoc ts)) = Node v (Assoc (map go ts))
  where
    go (k, t@(Node v' _)) = (f v v' k, mapEdges f t)

{-------------------------------------------------------------------------------
  Additional operations
-------------------------------------------------------------------------------}

-- | Limit the breath of a tree given a list of maximum breaths per level
limitBreath :: [Int] -> ETree k v -> ETree k (Maybe v)
limitBreath limits = go (limits ++ repeat maxBound) . numberLevels
  where
    go :: [Int] -> ETree k (v, Int) -> ETree k (Maybe v)
    go [] _ = error "the impossible happened"
    go (limit:limits') (Node (v, i) (Assoc ts))
      | i >= limit = Node Nothing (Assoc [])
      | otherwise  = Node (Just v) (Assoc (map (second (go limits')) ts))

numberLevels :: ETree k v -> ETree k (v, Int)
numberLevels = liftTree' Tree.numberLevels

markFirstChild :: ETree k v -> ETree k (v, Bool)
markFirstChild = liftTree' Tree.markFirstChild

{-------------------------------------------------------------------------------
  Interaction between ETree and Tree
-------------------------------------------------------------------------------}

-- | Push each edge label to a subtree into the node of that subtree
--
-- Since there is no edge to the root of the tree, the "edge" to that root
-- must be passed in as an argument.
pushEdges :: k -> ETree k v -> Tree (k, v)
pushEdges k (Node v (Assoc ts)) = Tree.Node (k, v) (map (uncurry pushEdges) ts)

-- | Inverse of 'pushEdges'
pullEdges :: Tree (k, v) -> (k, ETree k v)
pullEdges (Tree.Node (k, v) ts) = (k, Node v (Assoc (map pullEdges ts)))

-- | Lift an labelling function on trees to edged trees
liftTree :: (Tree (k, v) -> Tree ((k, v), b))
         -> k -> ETree k v -> ETree k (v, b)
liftTree f k = snd . pullEdges . fmap assoc . f . pushEdges k
  where
    assoc ((x, y), z) = (x, (y, z))

-- | Variation on 'liftTree' for functions which don't need the edges
liftTree' :: (forall a. Tree a -> Tree (a, b))
          -> ETree k v -> ETree k (v, b)
liftTree' f = snd . pullEdges . fmap assoc . f . pushEdges undefined
  where
    assoc ((x, y), z) = (x, (y, z))

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

_testETree :: ETree String String
_testETree =
  Node "a" (Assoc [ ("e1", Node "b" (Assoc [ ("e2", Node "d" (Assoc []))
                                           , ("e3", Node "e" (Assoc []))
                                           ])
                    )
                  , ("e4", Node "c" (Assoc [ ("e5", Node "f" (Assoc []))
                                           ])
                    )
                  ])
