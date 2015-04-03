-- | Exports most commonly used functionality
module Debug.Trace.Tree (
    -- * Labelled rose trees
    ETree(..)
    -- * Simple trees
  , SimpleTree
  , simpleETree
  , pattern SNode
  ) where

import Debug.Trace.Tree.Edged
import Debug.Trace.Tree.Simple
