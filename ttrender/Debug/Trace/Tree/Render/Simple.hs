-- | Render simple trees
module Debug.Trace.Tree.Render.Simple (renderTree) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo (B)
import Graphics.SVGFonts

import Debug.Trace.Tree.Simple
import Debug.Trace.Tree.Render.Options
import qualified Debug.Trace.Tree.Render.Edged as Edged

renderTree :: RenderOptions -> SimpleTree -> Diagram B
renderTree options =
      Edged.renderTree drawEdgeLabel (drawNode options)
    . applyOptions options

drawNode :: RenderOptions -> Maybe String -> Diagram B
drawNode RenderOptions{..} = go
  where
    go (Just str) = box str (lookup str renderColours)
    go Nothing    = mempty

drawEdgeLabel :: Maybe String -> Maybe String -> String -> (Diagram B, ArrowOpts Double)
drawEdgeLabel _ Nothing str = (
    stroke (textSVG' (textOpts 0.5) str) # fc black # lw none
  , arrOpts & shaftStyle %~ dashingG [0.1, 0.1] 0
            & headTexture .~ solid grey
  )
drawEdgeLabel _ (Just _) str = (
    stroke (textSVG' (textOpts 0.5) str) # fc black # lw none
  , arrOpts
  )

arrOpts :: ArrowOpts Double
arrOpts = with & headLength .~ normalized 0.008
               & shaftStyle %~ lw (normalized 0.001) . lc grey

textOpts :: Double -> TextOpts Double
textOpts h = with {
    textFont   = bit
  , textHeight = h
  }

box :: String -> Maybe (Colour Double) -> Diagram B
box str mc = mconcat [
      stroke txt # fc black # lw none
    , boundingBoxRect bb # setBg mc # lc black # lw (normalized 0.001)
    ]
  where
    txt :: Path V2 Double
    txt = textSVG' with str

    bb :: BoundingBox V2 Double
    bb = boundingBoxGrow 0.1 (boundingBox txt) <> boundingBox minBB

    minBB :: Diagram B
    minBB = square 1

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
