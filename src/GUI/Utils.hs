module GUI.Utils where

import SFML.Graphics qualified as SFML
import GUI.Geometry

toIntRect :: Rect Int -> SFML.IntRect 
toIntRect r =
  SFML.IntRect { ileft = getX l, itop = getY l, iwidth = getX d, iheight = getY d }
  where
  l = getRectLoc r
  d = getRectDim r
        