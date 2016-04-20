-- | Rendering constants
module Debug.Trace.Tree.Render.Constants where

-- | Minimum distance between nodes in the tree
constTreeHSep, constTreeVSep :: Double
constTreeHSep = 25
constTreeVSep = 25

-- | Extra space left between (the side of) edge labels and (a point on)
-- the lines that make up the arrows in the tree
constEdgePadding :: Double
constEdgePadding = 10

-- | Text height for node and edge labels
constNodeLabelHeight, constEdgeLabelHeight :: Double
constNodeLabelHeight = 25
constEdgeLabelHeight = 10

-- | Text height when overlaying coordinates
constCoordsOverlay :: Double
constCoordsOverlay = 35

-- | Length of the dashes to missing nodes
constMissingNodeDash :: Double
constMissingNodeDash = 5

-- | Arrow head length and line width
constArrowHeadLength, constArrowLineWidth :: Double
constArrowHeadLength = 8
constArrowLineWidth  = 1

-- | Padding around nodes
constNodePadding :: Double
constNodePadding = 5
