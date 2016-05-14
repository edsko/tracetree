module Debug.Trace.Tree.Render.Edged (renderTree) where

import Data.Bifunctor
import Data.Maybe (fromMaybe)
import Diagrams.Backend.Cairo (B)
import Diagrams.Prelude hiding (coords, offset)
import Diagrams.TwoD.Layout.Tree (SymmLayoutOpts)
import Graphics.SVGFonts
import qualified Diagrams.TwoD.Layout.Tree as Diagrams.Tree

import Debug.Trace.Tree.Edged
import Debug.Trace.Tree.Render.Constants

{-------------------------------------------------------------------------------
  Main rendering algorithm
-------------------------------------------------------------------------------}

-- | Node IDs allocated by 'Diagrams.Tree.label'
type PID = Int

renderTree :: forall k v.
              (v -> v -> k -> (Diagram B, ArrowOpts Double))
           -> (v -> Diagram B)
           -> Bool                  -- ^ Show coordinates?
           -> ETree k (v, Metadata) -- ^ Tree with metadata
           -> Diagram B
renderTree drK' drV' showCoords t =
    addArrows (arrows positioned) (nodes positioned)
  where
    -- Step 1. Render the individual tree nodes and the annotations on the edges
    drawn :: ETree (Diagram B, ArrowOpts Double) (Diagram B, Metadata)
    drawn = second drV . mapEdges drK $ t
      where
        drK :: (v, meta) -> (v, meta) -> k -> (Diagram B, ArrowOpts Double)
        drK (v, _) (v', _) k = drK' v v' k

        drV :: (v, Metadata) -> (Diagram B, Metadata)
        drV (v, meta) =
          let rendered | showCoords = renderCoords (coords meta) <> drV' v
                       | otherwise  = drV' v
          in (rendered, meta)

    -- Step 2. Assign IDs to all nodes and pair each node with its ID
    labelled :: ETree (Diagram B, ArrowOpts Double) (Diagram B, Metadata, PID)
    labelled = fmap (\((d, meta), n) -> (d # named n, meta, n))
             $ Diagrams.Tree.label drawn -- pair with unique IDs

    -- Step 3. Compute tree layout and move all nodes to their final location
    positioned :: ETree (Diagram B, ArrowOpts Double) (Diagram B, Metadata, PID)
    positioned = fmap (\((d, meta, n), p) -> (d # moveTo p, meta, n))
               $ symmLayout'' symmOpts (mempty, with)
               $ labelled

    -- Step 4. Extract nodes from the positioned tree
    --(Just extract all nodes and mappend them all together)
    nodes :: ETree a (Diagram B, meta, pid) -> Diagram B
    nodes = foldMap $ \(node, _meta, _n) -> node

    -- Step 5. Extract arrows from the positioned tree
    arrows :: ETree (Diagram B, ArrowOpts Double) (a, Metadata, PID) -> Arrows
    arrows = mconcat . keys . mapEdges aux
      where
        aux :: (a, Metadata, PID) -> (a, Metadata, PID)
            -> (Diagram B, ArrowOpts Double) -> Arrows
        aux (_, _parent, n) (_, child, n') (lbl, arrOpts) =
          connectLabelled arrOpts lbl (isFirstChild child) n n'

    symmOpts :: SymmLayoutOpts Double ((Diagram B, ArrowOpts Double), (Diagram B, Metadata, PID))
    symmOpts = with & Diagrams.Tree.slWidth  .~ computeWidth
                    & Diagrams.Tree.slHeight .~ computeHeight
                    & Diagrams.Tree.slHSep   .~ constTreeHSep
                    & Diagrams.Tree.slVSep   .~ constTreeVSep
      where
        -- We don't know where the label will be placed. In the worst case,
        -- the label is positioned to start or end precisely at the center
        -- of the node, so we allow for the worst case here.
        computeWidth ((edge, _opts), (node, _meta, _n)) =
          let (edgeMinX, edgeMaxX) = fromMaybe (0,0) $ extentX edge
              (nodeMinX, nodeMaxX) = fromMaybe (0,0) $ extentX node
              edgeWidth = edgeMaxX - edgeMinX
          in (negate edgeWidth `min` nodeMinX, edgeWidth `max` nodeMaxX)

        -- For the height we ignore the edge labels
        computeHeight ((_edge, _opts), (node, _meta, _n)) =
          fromMaybe (0,0) $ extentY node

-- | Lift standard layout algorithm to edged trees
symmLayout'' :: SymmLayoutOpts Double (k, v) -> k -> ETree k v -> ETree k (v, P2 Double)
symmLayout'' opts = liftTree (Diagrams.Tree.symmLayout' opts)

-- | Render coordinates
--
-- Note that these coordinates are not expected ever in the final diagram; they
-- are used only during diagram construction. So we don't need to worry too much
-- about how exactly they look.
renderCoords :: Coords -> Diagram B
renderCoords Coords{..} =
      stroke (textSVG (show (depth, offset)) constCoordsOverlay)
    # fc red
    # lw (global 0.5)

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
                 then translateX (negate constEdgePadding) . alignR
                 else translateX         constEdgePadding  . alignL
              )
            # moveTo midpoint
    in
      atop (lbl <> arrowBetween' opts s' e')
