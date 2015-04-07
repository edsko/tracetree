-- | Render simple trees
module Main (main) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo (B)
import Diagrams.Backend.Cairo.CmdLine (mainWith)
import Text.JSON

import Debug.Trace.Tree.Render.Simple
import Debug.Trace.Tree.Render.Options

{-------------------------------------------------------------------------------
  Main application driver
-------------------------------------------------------------------------------}

-- | Make sure the diagram is a reasonable size by default
--
-- (of course, user can override using -w command line option, but when
-- generating a number of diagrams of different sizes having a good
-- _relative_ default (relative to the size of the diagram) is very useful
-- to make sure the diagrams look uniform.
defaultScaleFactor :: Double
defaultScaleFactor = 25

generateDiagram :: RenderOptions -> IO (Diagram B)
generateDiagram options@RenderOptions{..} = do
  json <- readFile renderInput
  case decode json of
    Ok parsed -> return $ scale defaultScaleFactor $ renderTree options parsed
    Error err -> fail err

main :: IO ()
main = mainWith generateDiagram
