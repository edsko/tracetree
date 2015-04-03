-- | Additional operations on standard rose trees
module Debug.Trace.Tree.Rose (
    markSpine
  , markFirstChild
  , numberLevels
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

-- | Level numbering
--
-- Like breath-first numbering, but reset the counter at each level, i.e. given
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
--          / \
-- >    (B,0)  (C,1)
-- >     / \      \
-- > (D,0)  (E,1)  (F,2)
numberLevels :: Tree a -> Tree (a, Int)
numberLevels t = evalState (unfoldTreeM_BF unTree (markSpine t)) undefined
  where
    unTree :: MonadState Int m => Tree (a, Bool) -> m ((a, Int), [Tree (a, Bool)])
    unTree (Node (a, isSpine) ts) = do
      when isSpine $ put 0
      i <- state $ \i -> (i, i + 1)
      return ((a, i), ts)

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
