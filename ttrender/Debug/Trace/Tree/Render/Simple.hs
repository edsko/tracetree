-- | Render simple trees
module Debug.Trace.Tree.Render.Simple (renderTree) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo (B)
import Graphics.SVGFonts
import Graphics.SVGFonts.ReadFont (PreparedFont)
import Text.Regex.Posix ((=~))

import Debug.Trace.Tree.Simple
import Debug.Trace.Tree.Render.Options
import Debug.Trace.Tree.Render.Constants
import qualified Debug.Trace.Tree.Render.Edged as Edged

renderTree :: RenderOptions -> SimpleTree -> Diagram B
renderTree options@RenderOptions{..} =
      Edged.renderTree drawEdgeLabel (drawNode options) renderShowCoords
    . applyOptions options

drawNode :: RenderOptions -> Maybe String -> Diagram B
drawNode RenderOptions{..} node =
      maybeVertical node
    $ makeBox node
  where
    makeBox (Just str) = box str (lookup str renderColours)
    makeBox Nothing    = mempty

    maybeVertical Nothing    = id
    maybeVertical (Just str) = if any (\regexp -> str =~ regexp) renderVertical
                                 then rotateBy (1/4)
                                 else id

drawEdgeLabel :: Maybe String -> Maybe String -> String -> (Diagram B, ArrowOpts Double)
drawEdgeLabel _ Nothing str = (
    stroke (textSVG' (textOpts constEdgeLabelHeight bit) str) # fc black # lw none
  , arrOpts & shaftStyle %~ dashingG [constMissingNodeDash, constMissingNodeDash] 0
            & headTexture .~ solid grey
  )
drawEdgeLabel _ (Just _) str = (
    stroke (textSVG' (textOpts constEdgeLabelHeight bit) str) # fc black # lw none
  , arrOpts
  )

-- NOTE: global sizes are relative to the final vector space of the diagram,
-- and are not affected by scaling. We don't want to use normalized because
-- normalized values depend on the size of the final diagram, and the size of
-- the diagram may very greatly depending on the size of the tree.
-- See Section 2.5, "Measurement units" of the diagrams manual.
arrOpts :: ArrowOpts Double
arrOpts = with & headLength .~ global constArrowHeadLength
               & shaftStyle %~ lw (global constArrowLineWidth) . lc grey

textOpts :: Double -> PreparedFont Double -> TextOpts Double
textOpts h fnt = with {
    textFont   = fnt
  , textHeight = h
  }

box :: String -> Maybe (Colour Double) -> Diagram B
box str mc = mconcat [
      stroke txt # fc black # lw none
    , boundingBoxRect bb # setBg mc # lc black # lw (global 1)
    ]
  where
    txt :: Path V2 Double
    txt = textSVG' (textOpts constNodeLabelHeight lin) str

    bb :: BoundingBox V2 Double
    bb = boundingBoxGrow constNodePadding (boundingBox txt) <> boundingBox minBB

    minBB :: Diagram B
    minBB = square constNodeLabelHeight

    setBg (Just c) = fc c
    setBg Nothing  = id

{-------------------------------------------------------------------------------
  Auxiliary: operations on bounding boxes
-------------------------------------------------------------------------------}

boundingBoxGrow :: Double -> BoundingBox V2 Double -> (BoundingBox V2 Double)
boundingBoxGrow padding bb =
    fromCorners (p2 (x  - padding, y  - padding))
                (p2 (x' + padding, y' + padding))
  where
    (unp2 -> (x, y), unp2 -> (x', y')) = getCorners' bb

boundingBoxRect :: BoundingBox V2 Double -> Diagram B
boundingBoxRect = uncurry rect . boundingBoxSize

boundingBoxSize :: Num a => BoundingBox V2 a -> (a, a)
boundingBoxSize bb = ((x' - x), (y' - y))
  where
    (unp2 -> (x, y), unp2 -> (x', y')) = getCorners' bb

getCorners' :: Num a => BoundingBox V2 a -> (Point V2 a, Point V2 a)
getCorners' bb =
    case getCorners bb of
      Just (ll, ur) -> (ll     , ur    )
      Nothing       -> (origin , origin)
