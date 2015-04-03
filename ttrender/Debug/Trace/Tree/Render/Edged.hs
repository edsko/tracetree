module Debug.Trace.Tree.Render.Edged (renderTree) where

import Data.Bifunctor
import Data.Maybe (fromMaybe)
import Diagrams.Backend.Cairo (B)
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree hiding (renderTree)

import Debug.Trace.Tree.Edged

{-------------------------------------------------------------------------------
  Main rendering algorithm
-------------------------------------------------------------------------------}

renderTree :: forall k v.
              (v -> v -> k -> (Diagram B, ArrowOpts Double))
           -> (v -> Diagram B)
           -> ETree k v
           -> Diagram B
renderTree drK drV t =
    addArrows (arrows positioned) nodes
  where
    nodes :: Diagram B
    nodes = foldMap fst positioned

    positioned :: ETree (Diagram B, ArrowOpts Double) (Diagram B, Int)
    positioned = fmap (\((d, n), p) -> (d # moveTo p, n))
               $ symmLayout'' symmOpts (mempty, with)
               $ labelled

    labelled :: ETree (Diagram B, ArrowOpts Double) (Diagram B, Int)
    labelled = fmap (\(d, n) -> (d # named n, n)) $ label drawn

    drawn :: ETree (Diagram B, ArrowOpts Double) (Diagram B)
    drawn = second drV . mapEdges drK $ t

    arrows :: forall a. ETree (Diagram B, ArrowOpts Double) (a, Int) -> Arrows
    arrows = mconcat . keys . mapEdges aux . markFirstChild
      where
        aux :: ((a, Int), Bool) -> ((a, Int), Bool)
            -> (Diagram B, ArrowOpts Double) -> Arrows
        aux ((_, n), _parentIsFirst) ((_, n'), childIsFirst) (lbl, arrOpts) =
          connectLabelled arrOpts lbl childIsFirst n n'

    symmOpts :: SymmLayoutOpts Double ((Diagram B, ArrowOpts Double), (Diagram B, Int))
    symmOpts = with & slWidth  .~ computeWidth
                    & slHeight .~ computeHeight
      where
        -- We don't know where the label will be placed. In the worst case,
        -- the label is positioned to start or end precisely at the center
        -- of the node, so we allow for the worst case here.
        computeWidth ((edge, _opts), (node, _)) =
          let (edgeMinX, edgeMaxX) = fromMaybe (0,0) $ extentX edge
              (nodeMinX, nodeMaxX) = fromMaybe (0,0) $ extentX node
              edgeWidth = edgeMaxX - edgeMinX
          in (negate edgeWidth `min` nodeMinX, edgeWidth `max` nodeMaxX)

        -- For the height we ignore the edge labels
        computeHeight ((_edge, _opts), (node, _)) =
          fromMaybe (0,0) $ extentY node

-- | Lift standard layout algorithm to edged trees
symmLayout'' :: SymmLayoutOpts Double (k, v) -> k -> ETree k v -> ETree k (v, P2 Double)
symmLayout'' opts = liftTree (symmLayout' opts)

{-------------------------------------------------------------------------------
  Diagrams auxiliary: labelled arrows
-------------------------------------------------------------------------------}

newtype Arrows = Arrows { addArrows :: Diagram B -> Diagram B }

instance Monoid Arrows where
  mempty = Arrows id
  Arrows f `mappend` Arrows g = Arrows (f . g)

-- based on 'connectOutside'
connectLabelled :: ArrowOpts Double -> Diagram B -> Bool -> Int -> Int -> Arrows
connectLabelled opts edgeLabel labelOnLeft n1 n2 = Arrows $
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
    let v = location b2 .-. location b1
        midpoint = location b1 .+^ (v/2)
        s' = fromMaybe (location b1) $ traceP midpoint (-v) b1
        e' = fromMaybe (location b2) $ traceP midpoint v b2
        lbl = edgeLabel
            # (if labelOnLeft
                 then translateX (-0.5) . alignR
                 else translateX   0.5  . alignL
              )
            # moveTo midpoint
    in
      atop (lbl <> arrowBetween' opts s' e')
