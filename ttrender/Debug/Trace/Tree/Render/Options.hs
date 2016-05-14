-- | Render simple trees
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Debug.Trace.Tree.Render.Options where -- (RenderOptions(..), applyOptions) where

import Data.Bifunctor
import Data.Colour (Colour)
import Data.Colour.Names (readColourName)
import Data.Maybe (isJust)
import Data.List (groupBy)
import Data.Function (on)
import Diagrams.Backend.CmdLine (Parseable(..))
import Options.Applicative
import Text.Regex.Posix ((=~))
import qualified Text.Parsec        as Parsec
import qualified Text.Parsec.String as Parsec

import Debug.Trace.Tree.Simple (SimpleTree, simpleETree)
import Debug.Trace.Tree.Edged
import Debug.Trace.Tree.Assoc (Assoc(..))
import qualified Debug.Trace.Tree.Simple as Simple

{-------------------------------------------------------------------------------
  Matching nodes
-------------------------------------------------------------------------------}

data RegExp = RegExp String

instance Show RegExp where
    show (RegExp regExp) = regExp
    
instance MatchAgainst String RegExp where
    s `matchAgainst` (RegExp regExp) = s =~ regExp

{-------------------------------------------------------------------------------
  Command line args proper
-------------------------------------------------------------------------------}

data RenderOptions = RenderOptions {
    renderHideNodes    :: [Hide String]
  , renderMerge        :: [String]
  , renderVertical     :: [RegExp]
  , renderColours      :: [(String, Colour Double)]
  , renderMaxNotShown  :: Int
  , renderDelChildren  :: [String]
  , renderShowCoords   :: Bool
  , renderInput        :: FilePath
  }

instance Parseable RenderOptions where
  parser = RenderOptions
    <$> ( many (option (readParsec parseHide) $ mconcat [
            long "hide"
          , metavar "HIDE"
          , help $ unlines [
                "Hide certain nodes in the tree; arrows to these nodes are shown as dangling (modulo the max-not-shown option)."
              , "Can be used multiple times."
              , "Valid syntax for the argument is: "
              , "  \"node(y,x)\": Hide the node at the specified coordinates."
              , "  \"match(REGEXP)\": Hide any node matching the specified REGEXP."
              , "  \"max(REGEXP,INT)\": Limit the number of children of any node matching REGEXP to INT."
              ]
          ]))
    <*> ( many (strOption $ mconcat [
            long "merge"
          , metavar "C"
          , help "Collapse any tree of shape (C' .. (C args) ..) to (C' .. args ..). Can be used multiple times."
          ]))
    <*> ( many (option (readParsec parseRegExp) $ mconcat [
            long "vertical"
          , metavar "REGEXP"
          , help "Show any node matching the specified regular expression vertically. Can be used multiple times."
          ]))
    <*> ( many (option readColourAssignment $ mconcat [
            long "colour"
          , help "Set the colour for a node. For example --colour P=red sets the background of all P nodes to be red. Can be used multiple times."
          ]))
    <*> ( option auto $ mconcat [
             long "max-not-shown"
           , metavar "Int"
           , value 3
           , showDefault
           , help "Maximum number of edges to hidden nodes (use together with --max-breadths)"
           ])
    <*> ( many (strOption $ mconcat [
             long "delete-children"
           , help "Delete the children of the specified node. This actually removes the children from the tree, no dangling arrows are shown. Can be used multiple times."
           ]))
    <*> ( switch $ mconcat [
             long "show-coords"
           , help "Superimpose node coordinates when rendering the tree. Useful when figuring out which arguments to pass to --hide."
           ])
    <*> ( argument str (metavar "JSON") )

readParsec :: Parsec.Parser a -> ReadM a
readParsec p = do
    arg <- str
    case Parsec.parse p "command line argument" arg of
      Left  err -> fail (show err)
      Right a   -> return a

readColourAssignment :: ReadM (String, Colour Double)
readColourAssignment = do
    arg <- str
    case break (== '=') arg of
      (constr, '=':colour) -> (constr,) <$> readColourName colour
      _ -> fail "Invalid colour assignment"

applyOptions :: RenderOptions -> SimpleTree -> ETree String (Maybe String, Metadata)
applyOptions RenderOptions{..} =
      applyMaxNotShown renderMaxNotShown
    . hideNodes renderHideNodes
    . annotate
    . simpleETree
    . applyMerge renderMerge
    . applyDelChildren renderDelChildren

applyMerge :: [String] -> SimpleTree -> SimpleTree
applyMerge toMerge (Simple.Node c' (Assoc ts)) =
    Simple.Node c' . fmap (applyMerge toMerge) . Assoc $ concatMap aux ts
  where
    aux t@(_, Simple.Node c (Assoc ts'))
      | c `elem` toMerge = ts'
      | otherwise        = [t]
    aux _ = error "inaccessible"
applyMerge _ _ = error "inaccessible"

applyDelChildren :: [String] -> SimpleTree -> SimpleTree
applyDelChildren toHide = go
  where
    go (Simple.Node c (Assoc ts))
      | c `elem` toHide = Simple.Node c (Assoc [])
      | otherwise       = Simple.Node c (Assoc (map (second go) ts))
    go _ = error "inaccessible"

applyMaxNotShown :: Int -> ETree String (Maybe String, Metadata) -> ETree String (Maybe String, Metadata)
applyMaxNotShown n = \(Node c (Assoc ts)) ->
    let culled = concatMap aux (groupBy ((==) `on` isShown) ts)
    in Node c $ fmap (applyMaxNotShown n) (Assoc culled)
  where
    -- Replace each group of hidden nodes with an ellipsis (if larger than n)
    -- We re-use the node metadata of the first node in the group
    aux :: [(String, ETree String (Maybe String, Metadata))]
        -> [(String, ETree String (Maybe String, Metadata))]
    aux [] = error "impossible: groupBy does not create empty groups"
    aux ts@(t:_)
      | not (isShown t) && length ts > n = [ellipsis (rootMeta (snd t))]
      | otherwise                        = ts

    ellipsis :: Metadata -> (String, ETree String (Maybe String, Metadata))
    ellipsis meta = ("...", Node (Nothing, meta) (Assoc []))

    isShown :: (String, ETree String (Maybe String, meta)) -> Bool
    isShown (_key, (Node (c', _coords) _subtree)) = isJust c'

    rootMeta :: ETree k (v, Metadata) -> Metadata
    rootMeta (Node (_, meta) _) = meta

{-------------------------------------------------------------------------------
  Parser for Hide
-------------------------------------------------------------------------------}

parseHide :: Parsec.Parser (Hide String)
parseHide =
      (do Parsec.try $ Parsec.string "node("
          s <- parseNodeSpec
          Parsec.string ")"
          return $ HideNode s)
  <|> (do Parsec.try $ Parsec.string "max("
          n <- parseInt
          Parsec.string ","
          s <- parseNodeSpec
          Parsec.string ")"
          return $ HideMax n s)

parseNodeSpec :: Parsec.Parser (NodeSpec String)
parseNodeSpec =
    Parsec.try (do y <- parseInt
                   Parsec.string ","
                   x <- parseInt
                   return $ NodeCoords $ Coords y x)
  <|>
    (NodeMatch <$> parseRegExp)

parseInt :: Parsec.Parser Int
parseInt = read <$> Parsec.many1 Parsec.digit

-- | Regular expression delimited by an unmatched closing parenthesis
parseRegExp :: Parsec.Parser RegExp
parseRegExp = RegExp <$> matchBrackets

{-------------------------------------------------------------------------------
  Auxiliary parsec
-------------------------------------------------------------------------------}

-- | Match as much as possible, until we see an unexpected closing parenthesis
matchBrackets :: Parsec.Parser String
matchBrackets = go 0
  where
    go :: Int -> Parsec.Parser String
    go nestingDepth = (do
      x  <- if nestingDepth > 0 then Parsec.anyChar else Parsec.noneOf ")"
      let nestingDepth' | x == '('  = nestingDepth + 1
                        | x == ')'  = nestingDepth - 1
                        | otherwise = nestingDepth
      xs <- go nestingDepth'
      return (x:xs)) <|> return ""
