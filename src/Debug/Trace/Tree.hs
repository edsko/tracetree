-- | Exports most commonly used functionality
module Debug.Trace.Tree (
    -- * Labelled rose trees
    ETree(..)
    -- * Simple trees
  , SimpleTree(..)
    -- * Generic generation
  , GSimpleTree(..)
    -- * Trace operations
  , gtraceJson
  ) where

import System.IO.Unsafe
import Text.JSON

import Debug.Trace.Tree.Edged
import Debug.Trace.Tree.Simple
import Debug.Trace.Tree.Generic

{-------------------------------------------------------------------------------
  Tracing operations
-------------------------------------------------------------------------------}

gtraceJson :: GSimpleTree b => FilePath -> (a -> b) -> a -> a
gtraceJson fp f x = unsafePerformIO $ do
    writeFile fp (encode . showJSON . fromGeneric . f $ x)
    return x
