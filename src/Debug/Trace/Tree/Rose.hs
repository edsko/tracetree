-- | Additional operations on standard rose trees
module Debug.Trace.Tree.Rose (
    -- * Annotating nodes
    Depth
  , Offset
  , Coords(..)
  , Metadata(..)
  , annotate
  ) where

import Control.Monad.State
import Data.Tree

{-------------------------------------------------------------------------------
  Annotating the tree
-------------------------------------------------------------------------------}

-- | Depth of a node in the tree
type Depth = Int

-- | Offset of a node in the tree
--
-- This is the horizontal offset of a node across all nodes at that depth.
--
-- For example, the offsets of
--
-- >     A
-- >    / \
-- >   B   C
-- >  / \   \
-- > D   E   F
--
-- are given by
--
-- >       (A,0)
-- >        / \
-- >    (B,0)  (C,1)
-- >     / \      \
-- > (D,0)  (E,1)  (F,2)
--
-- Similarly, the offsets of
--
-- >   A
-- >  / \
-- > B   C
-- >    / \
-- >   D   E
--
-- are given by
--
-- >    (A,0)
-- >     / \
-- > (B,0) (C,1)
-- >       /   \
-- >   (D,0)   (E,1)
--
-- Note that in this second example, D gets number 0 because it's the first
-- node at this level; it's therefore not the case that the nodes with number 0
-- necessarily make up the _spine_ of the tree.
type Offset = Int

-- | Coordinates of a node in the tree
data Coords = Coords {
      depth  :: Depth  -- ^ The "y coordinate" (depth in the tree)
    , offset :: Offset -- ^ The "x coordinate" (across all nodes at this depth)
    }
  deriving (Show, Eq, Ord)

-- | Metadata of a node in the tree
data Metadata = Metadata {
      isSpine      :: Bool
    , isFirstChild :: Bool
    , coords       :: Coords
    }
  deriving (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Auxiliary: operations on trees
-------------------------------------------------------------------------------}

-- | Mark the spine of a tree
markSpine :: Tree a -> Tree (a, Bool)
markSpine = go True
  where
    go :: Bool -> Tree a -> Tree (a, Bool)
    go isSpine (Node a ts) = Node (a, isSpine)
                           $ map (uncurry go) $ zip (isSpine : repeat False) ts

-- | Mark the first child of each node
markFirstChild :: Tree a -> Tree (a, Bool)
markFirstChild = go True
  where
    go :: Bool -> Tree a -> Tree (a, Bool)
    go isFirst (Node a ts) = Node (a, isFirst)
                           $ map (uncurry go) $ zip (True : repeat False) ts

-- | Mark each node with its depth in the tree
markDepth :: Tree a -> Tree (a, Depth)
markDepth = go 0
  where
    go :: Depth -> Tree a -> Tree (a, Depth)
    go d (Node a ts) = Node (a, d) $ map (go (d + 1)) ts

-- | Mark each node with its coordinates
markCoords :: forall a. Tree a -> Tree (a, Coords)
markCoords t = evalState (unfoldTreeM_BF go (markDepth t)) (Coords 0 0)
  where
    go :: MonadState Coords m
       => Tree (a, Depth) -> m ((a, Coords), [Tree (a, Depth)])
    go (Node (a, depth) ts) = do
      cs <- state $ \(Coords curDepth curOffset) ->
              if depth > curDepth -- first node at the next depth level?
                then (Coords depth 0         , Coords depth 1)
                else (Coords depth curOffset , Coords depth (curOffset + 1))
      return ((a, cs), ts)

annotate :: Tree a -> Tree (a, Metadata)
annotate = fmap aux
         . markCoords
         . markFirstChild
         . markSpine
  where
    aux :: (((a, Bool), Bool), Coords) -> (a, Metadata)
    aux (((a, isSpine), isFirstChild), coords) = (a, Metadata{..})

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

_testTree :: Tree String
_testTree =
  Node "a" [ Node "b" [ Node "d" []
                      , Node "e" []
                      ]
           , Node "c" [ Node "f" []
                      ]
           ]

-- Tree with deeper nodes on the right than on the left
_testTree2 :: Tree String
_testTree2 =
  Node "a" [ Node "b" []
           , Node "c" [ Node "d" []
                      , Node "e" []
                      ]
           ]
