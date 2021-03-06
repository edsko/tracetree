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

generateDiagram :: RenderOptions -> IO (Diagram B)
generateDiagram options@RenderOptions{..} = do
  json <- readFile renderInput
  case decode json of
    Ok parsed -> return $ renderTree options parsed
    Error err -> fail err

main :: IO ()
main = mainWith generateDiagram
