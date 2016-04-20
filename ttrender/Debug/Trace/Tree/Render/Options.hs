-- | Render simple trees
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Debug.Trace.Tree.Render.Options (RenderOptions(..), applyOptions) where

import Data.Bifunctor
import Data.Colour (Colour)
import Data.Colour.Names (readColourName)
import Diagrams.Backend.CmdLine (Parseable(..))
import Options.Applicative
import qualified Text.Parsec        as Parsec
import qualified Text.Parsec.String as Parsec

import Debug.Trace.Tree.Simple (SimpleTree, simpleETree)
import Debug.Trace.Tree.Edged (ETree, Hide(..))
import Debug.Trace.Tree.Assoc (Assoc(..))
import qualified Debug.Trace.Tree.Edged  as Edged
import qualified Debug.Trace.Tree.Simple as Simple

data RenderOptions = RenderOptions {
    renderHideNodes    :: [Hide]
  , renderMerge        :: [String]
  , renderVertical     :: [String]
  , renderColours      :: [(String, Colour Double)]
  , renderMaxNotShown  :: Int
  , renderDelChildren  :: [String]
  , renderShowCoords   :: Bool
  , renderInput        :: FilePath
  }

instance Parseable RenderOptions where
  parser = RenderOptions
    <$> ( many (option readHide $ mconcat [
            long "hide"
          , metavar "HIDE"
          , help "Hide certain nodes in the tree; arrows to these nodes are shown as dangling (modulo the max-not-shown option). Valid syntax for the argument is: \"node(y,x)\": Hide the node at the specified level. Can be used multiple times."
          ]))
    <*> ( many (strOption $ mconcat [
            long "merge"
          , metavar "C"
          , help "Collapse any tree of shape (C' .. (C args) ..) to (C' .. args ..). Can be used multiple times."
          ]))
    <*> ( many (strOption $ mconcat [
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

readHide :: ReadM Hide
readHide = do
    arg <- str
    case Parsec.parse parseHide "HIDE" arg of
      Left  err  -> fail (show err)
      Right hide -> return hide

readColourAssignment :: ReadM (String, Colour Double)
readColourAssignment = do
    arg <- str
    case break (== '=') arg of
      (constr, '=':colour) -> (constr,) <$> readColourName colour
      _ -> fail "Invalid colour assignment"

applyOptions :: RenderOptions -> SimpleTree -> ETree String (Maybe String, (Int, Int))
applyOptions RenderOptions{..} =
      applyMaxNotShown renderMaxNotShown
    . Edged.hideNodes renderHideNodes
    . Edged.markCoords
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

applyMaxNotShown :: Int -> ETree String (Maybe String, (Int, Int)) -> ETree String (Maybe String, (Int, Int))
applyMaxNotShown _ = id -- TODO: make work again now that we introduced --hide
{-
applyMaxNotShown n (Edged.Node c (Assoc ts)) =
    let (shown, notShown) = partition (isShown . snd) ts
        culled = if length notShown > n
                   then shown ++ take n notShown ++ [ellipsis]
                   else ts
    in Edged.Node c $ fmap (applyMaxNotShown n) (Assoc culled)
  where
    isShown :: ETree String (Maybe String) -> Bool
    isShown (Edged.Node c' _) = isJust c'

    ellipsis :: (String, ETree String (Maybe String))
    ellipsis = ("...", Edged.Node Nothing (Assoc []))
-}

{-------------------------------------------------------------------------------
  Parser for Hide
-------------------------------------------------------------------------------}

parseHide :: Parsec.Parser Hide
parseHide = parseHideNode

parseHideNode :: Parsec.Parser Hide
parseHideNode = do
    Parsec.string "node("
    y <- Parsec.many1 Parsec.digit
    Parsec.string ","
    x <- Parsec.many1 Parsec.digit
    Parsec.string ")"
    return $ HideNode (read y) (read x)
