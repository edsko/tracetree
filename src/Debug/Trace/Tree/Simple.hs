module Debug.Trace.Tree.Simple (
    SimpleTree
  , simpleETree
  , pattern SNode
  ) where

import Text.JSON
import Debug.Trace.Tree.Edged
import Debug.Trace.Tree.Assoc

{-------------------------------------------------------------------------------
  Trees containing only strings
-------------------------------------------------------------------------------}

newtype SimpleTree = SimpleTree { simpleETree :: ETree String String }

pattern SNode :: String -> Assoc String SimpleTree -> SimpleTree
pattern SNode v ts <- SimpleTree (ENode v (fmap SimpleTree -> ts))
  where
    SNode v ts = SimpleTree (ENode v (fmap simpleETree ts))

{-------------------------------------------------------------------------------
  Serialization to and from JSON
-------------------------------------------------------------------------------}

instance JSON SimpleTree where
  showJSON (SNode v ts) = JsonTree v $ fmap showJSON ts
  showJSON _ = error "inaccessible"

  readJSON (JsonTree v ts) = SNode v <$> traverse readJSON ts
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
