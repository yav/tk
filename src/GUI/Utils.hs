module GUI.Utils where

import SFML.Graphics qualified as SFML
import GUI.Geometry
import GUI.Color
import qualified SFML.System as SFML

toIntRect :: Rect Int -> SFML.IntRect 
toIntRect r =
  withVec' (rectLoc r) \x y ->
  withVec' (rectDim r) \w h ->
  SFML.IntRect { ileft = x, itop = y, iwidth = w, iheight = h }
{-# inline toIntRect #-}

toFloatVec :: Vec Float -> SFML.Vec2f
toFloatVec = withVec SFML.Vec2f
{-# inline toFloatVec #-}

toVertex :: Vec Float -> Color -> SFML.Vertex
toVertex v c = 
  SFML.Vertex {
    position = toFloatVec v,
    color = c,
    texCoords = 0
  }
{-# inline toVertex #-}
        