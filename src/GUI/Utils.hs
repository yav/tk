module GUI.Utils where

import SFML.Graphics qualified as SFML
import GUI.Geometry
import qualified SFML.System as SFML

toIntRect :: Rect Int -> SFML.IntRect 
toIntRect r =
  withVec' (rectLoc r) \x y ->
  withVec' (rectDim r) \w h ->
  SFML.IntRect { ileft = x, itop = y, iwidth = w, iheight = h }

toFloatVec :: Vec Float -> SFML.Vec2f
toFloatVec = withVec SFML.Vec2f
        