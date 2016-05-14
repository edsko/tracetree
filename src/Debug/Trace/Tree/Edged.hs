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
    -- * Hiding nodes
  , MatchAgainst(..)
  , NodeSpec(..)
  , Hide(..)
  , hideNodes
    -- * Annotation
  , Depth
  , Offset
  , Coords(..)
  , Metadata(..)
  , Rose.isFirstChild
  , annotate
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
import Debug.Trace.Tree.Rose (Depth, Offset, Coords(..), Metadata(..))
import qualified Debug.Trace.Tree.Rose as Rose

{-------------------------------------------------------------------------------
  Main datatype
-------------------------------------------------------------------------------}

-- | Tree with nodes labelled with @v@ and arrows labelled with @k@
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

annotate :: ETree k v -> ETree k (v, Metadata)
annotate = liftTree' Rose.annotate

{-------------------------------------------------------------------------------
  Hiding nodes
-------------------------------------------------------------------------------}

-- | Abstract definition of something we can match against a value
class MatchAgainst v m where
    matchAgainst :: v -> m -> Bool

-- | Various ways we can specify a node
data NodeSpec v =
    -- | Node at the specified coordinates
    NodeCoords Coords

    -- | Match the value of the node
  | forall m. (MatchAgainst v m, Show m) => NodeMatch m

matchSpec :: NodeSpec v -> (v, Metadata) -> Bool
matchSpec (NodeCoords c) (_, Metadata{..}) = c == coords
matchSpec (NodeMatch  m) (v, _)            = v `matchAgainst` m

-- | Specification of nodes to hide
data Hide v =
    -- | Hide the specified node
    HideNode (NodeSpec v)

    -- | Limit the range of children of the specified node
  | HideMax (Int, Int) (NodeSpec v)

instance Show (NodeSpec v) where
    show (NodeCoords Coords{..}) = show depth ++ "," ++ show offset
    show (NodeMatch  m)          = show m

instance Show (Hide v) where
    show (HideNode  spec) = "node(" ++ show spec ++ ")"
    show (HideMax n spec) = "max(" ++ show n ++ "," ++ show spec ++ ")"

-- | Check if a certain node should be hidden
isHidden :: forall v.
            [Hide v]            -- ^ User-specified rules for hiding nodes
         -> Maybe (v, Metadata) -- ^ Parent node
         -> (v, Metadata)       -- ^ This node
         -> Bool
isHidden rules mParent this@(_, Metadata{..}) =
    any (hides mParent) rules
  where
    hides :: Maybe (v, Metadata) -> Hide v -> Bool
    hides _             (HideNode  spec) = matchSpec spec this
    hides (Just parent) (HideMax r spec) = matchSpec spec parent && not (inRange r nthChild)
    hides Nothing       (HideMax _ _)    = False

    inRange :: (Int, Int) -> Int -> Bool
    inRange (lo, hi) n = lo <= n && n <= hi

hideNodes :: forall k v. [Hide v] -> ETree k (v, Metadata) -> ETree k (Maybe v, Metadata)
hideNodes spec = go Nothing
  where
    go :: Maybe (v, Metadata) -> ETree k (v, Metadata) -> ETree k (Maybe v, Metadata)
    go mParent (Node (v, meta) (Assoc ts))
      | isHidden spec mParent (v, meta) = Node (Nothing, meta) $ Assoc []
      | otherwise = Node (Just v, meta) $ Assoc (map (second (go (Just (v, meta)))) ts)

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
