-- | Render simple trees
module Debug.Trace.Tree.Render.Options (RenderOptions(..), applyOptions) where

import Data.Colour (Colour)
import Data.Colour.Names (readColourName)
import Data.List (partition)
import Data.Maybe (isJust)
import Diagrams.Backend.CmdLine (Parseable(..))
import Options.Applicative

import Debug.Trace.Tree.Simple (SimpleTree, simpleETree)
import Debug.Trace.Tree.Edged (ETree)
import Debug.Trace.Tree.Assoc (Assoc(..))
import qualified Debug.Trace.Tree.Edged  as Edged
import qualified Debug.Trace.Tree.Simple as Simple

data RenderOptions = RenderOptions {
    renderMaxBreadths        :: [Int]
  , renderCollapseSingletons :: Bool
  , renderColours            :: [(String, Colour Double)]
  , renderMaxNotShown        :: Int
  , renderInput              :: FilePath
  }

instance Parseable RenderOptions where
  parser = RenderOptions
    <$> ( option auto $ mconcat [
            long "max-breadths"
          , metavar "[Int]"
          , value []
          , help "Limit the breath at each level. For example, --max-breadths '[2,1]' means there will be at most 2 nodes at level 1, at most 1 nodes at level 2, and the remaining levels are unrestricted."
          ])
    <*> ( switch $ mconcat [
            long "collapse-singletons"
          , help "Collapse any tree of the form (C (C' args)) to (C args)"
          ])
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
    <*> ( argument str (metavar "JSON") )

readColourAssignment :: ReadM (String, Colour Double)
readColourAssignment = do
    arg <- str
    case break (== '=') arg of
      (constr, '=':colour) -> (constr,) <$> readColourName colour
      _ -> fail "Invalid colour assignment"

applyOptions :: RenderOptions -> SimpleTree -> ETree String (Maybe String)
applyOptions RenderOptions{..} =
      applyMaxNotShown renderMaxNotShown
    . Edged.limitBreath renderMaxBreadths
    . simpleETree
    . (if renderCollapseSingletons then collapseSingletons else id)

collapseSingletons :: SimpleTree -> SimpleTree
collapseSingletons (Simple.Node c (Assoc [("0", t)])) =
    let Simple.Node _c' ts = collapseSingletons t
    in Simple.Node c ts
collapseSingletons (Simple.Node c ts) =
    Simple.Node c (fmap collapseSingletons ts)
collapseSingletons _ =
    error "inaccessible"

applyMaxNotShown :: Int -> ETree String (Maybe String) -> ETree String (Maybe String)
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
