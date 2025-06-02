module GUI.Scene where

import GUI.Color
import GUI.Text

data Scene =
    Text String
  | Font Font Scene
  | FontSize Int Scene
  | FontStyle TextStyle Scene
  | FontColor Color Scene

  | Rectangle Float Float
  | FillColor Color Scene
  | OutlineColor Color Scene
  | Outline Float Scene

  | Scene :&: Scene
  | Blank

  | Translate Float Float Scene
  | Scale Float Float Scene
  | ScaleWithCenter Float Float Float Float Scene
  | Rotate Float Scene            -- in degree
  | RotateWithCenter Float Float Float Scene -- degree, x, y
