-- | Simple trees (edged trees with strings everywhere)
--
-- Intended to be double imported:
--
-- > import Debug.Trace.Tree.Simple
-- > import qualified Debug.Trace.Tree.Simple as Simple
module Debug.Trace.Tree.Simple (
    SimpleTree
  , simpleETree
  , pattern Node
  , pattern Leaf
  ) where

import Text.JSON
import Debug.Trace.Tree.Edged (ETree)
import Debug.Trace.Tree.Assoc
import qualified Debug.Trace.Tree.Edged as Edged

{-------------------------------------------------------------------------------
  Trees containing only strings
-------------------------------------------------------------------------------}

newtype SimpleTree = SimpleTree { simpleETree :: ETree String String }
  deriving (Show, Eq)

pattern Node :: String -> Assoc String SimpleTree -> SimpleTree
pattern Node v ts <- SimpleTree (Edged.Node v (fmap SimpleTree -> ts))
  where
    Node v ts = SimpleTree (Edged.Node v (fmap simpleETree ts))

pattern Leaf :: String -> SimpleTree
pattern Leaf v = Node v (Assoc [])

{-------------------------------------------------------------------------------
  Serialization to and from JSON
-------------------------------------------------------------------------------}

instance JSON SimpleTree where
  showJSON (Node v ts) = JsonTree v $ fmap showJSON ts
  showJSON _ = error "inaccessible"

  readJSON (JsonTree v ts) = Node v <$> traverse readJSON ts
  readJSON _ = fail "Invalid JSON"

{-------------------------------------------------------------------------------
  Auxiliary: patterns for working with JSValue
-------------------------------------------------------------------------------}

pattern JsonTree :: String -> Assoc String JSValue -> JSValue
pattern JsonTree v ts <- SingletonObject v (Object (Assoc -> ts))
  where
    JsonTree v ts = SingletonObject v (Object (assocList ts))

pattern SingletonObject :: String -> JSValue -> JSValue
pattern SingletonObject k v = Object [(k, v)]

pattern Object :: [(String, JSValue)] -> JSValue
pattern Object obj <- JSObject (fromJSObject -> obj)
  where
    Object obj = JSObject (toJSObject obj)
