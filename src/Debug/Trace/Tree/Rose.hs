-- | Additional operations on standard rose trees
module Debug.Trace.Tree.Rose (
    markSpine
  , markFirstChild
  , markDepth
  , numberLevels
  , markCoords
  ) where

import Control.Monad.State
import Data.Tree

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
markDepth :: Tree a -> Tree (a, Int)
markDepth = go 0
  where
    go :: Int -> Tree a -> Tree (a, Int)
    go d (Node a ts) = Node (a, d) $ map (go (d + 1)) ts

-- | Level numbering
--
-- Like breadth-first numbering, but reset the counter at each level, i.e. given
--
-- >     A
-- >    / \
-- >   B   C
-- >  / \   \
-- > D   E   F
--
-- return
--
-- >       (A,0)
-- >        / \
-- >    (B,0)  (C,1)
-- >     / \      \
-- > (D,0)  (E,1)  (F,2)
--
-- Similarly,
--
-- >   A
-- >  / \
-- > B   C
-- >    / \
-- >   D   E
--
-- becomes
--
-- >    (A,0)
-- >     / \
-- > (B,0) (C,1)
-- >       /   \
-- >   (D,0)   (E,1)
--
-- (note that in this second example, D gets number 0 because it's the first
-- node at this level; it's therefore not the case that the nodes with number 0
-- necessarily make up the _spine_ of the tree.)
numberLevels :: forall a. Tree a -> Tree (a, Int)
numberLevels t = evalState (unfoldTreeM_BF unTree (markDepth t)) (0, 0)
  where
    unTree :: MonadState (Depth, Index) m
           => Tree (a, Depth) -> m ((a, Index), [Tree (a, Depth)])
    unTree (Node (a, depth) ts) = do
      i <- state $ \(curDepth, curIdx) ->
             if depth > curDepth
               then (0, (depth, 1))
               else (curIdx, (curDepth, curIdx + 1))
      return ((a, i), ts)

-- | Mark each node with its (y, x) coordinates
--
-- This is the combination of 'markDepth' and 'numberLevels'
markCoords :: Tree a -> Tree (a, (Int, Int))
markCoords = fmap reassoc . numberLevels . markDepth
  where
    reassoc :: ((a, Int), Int) -> (a, (Int, Int))
    reassoc ((a, y), x) = (a, (y, x))

type Depth = Int
type Index = Int

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
