module GUI.Scene where

import GUI.Color
import GUI.Text
import GUI.Geometry
import GUI.Texture

-- | Describes scenes.  The coordinate system has the X axis going to the
-- right and, and the Y axis going down.
data Scene =
    Text String                 -- ^ Draw some text
  | Font Font Scene             -- ^ Set the font
  | FontSize Int Scene          -- ^ Set the font size
  | FontStyle TextStyle Scene   -- ^ Set the text style
  | FontColor Color Scene       -- ^ Set the text color

  | Line MultiLine              -- ^ Draw a line of thickness 1.
                                -- Uses outline color by default.

  | Rectangle Float Float       -- ^ Rectangle with given width and height
  | Circle Float                -- ^ Circle with the given radius
  | Sprite                      -- ^ Draw a sprite

  | FillColor Color Scene       -- ^ Set the fill color for shapes
  | OutlineColor Color Scene    -- ^ Set the color of a shape's outline
  | Outline Float Scene         -- ^ Set the thickness of a shape's outline

  | Texture Texture (Maybe (Rect Int)) Scene
    -- ^ Set the texture for a shape.
    -- The optional rectangle specify a sub-region of the texture to use.

  | Scene :&: Scene
    -- Multi part scene. The right one is drawn on top.

  | Blank
    -- ^ Empty scene

  | Translate {-# UNPACK #-} !(Vec Float) Scene
    -- ^ Translate a scene

  | Scale Float Float Scene
  | ScaleWithCenter {-# UNPACK #-} !(Vec Float) Float Float Scene
  | Rotate Float Scene
    -- ^ Rotate around the origin by the gien degrees.
    -- The rotation is clockwise (X axis to Y axis).

  | RotateWithCenter {-# UNPACK #-} !(Vec Float) Float Scene
    -- degree, x, y

line :: Line Float -> Scene
line = withLine \start end -> Line (LineTo start (LineTo end LineEnd))

data MultiLine =
    LineColor Color MultiLine
  | LineTo {-# UNPACK #-} !(Vec Float) MultiLine
  | LineToRel {-# UNPACK #-} !(Vec Float) MultiLine
  | LineEnd