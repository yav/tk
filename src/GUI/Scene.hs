module GUI.Scene where

import GUI.Color
import GUI.Text
import GUI.Geometry
import GUI.Texture

-- | Describes scenes.  The coordinate system has the X axis going to the
-- right and, and the Y axis going down.
data Scene =
    DrawText String                 -- ^ Draw some text
  | SetFont Font Scene             -- ^ Set the font
  | SetFontSize Int Scene          -- ^ Set the font size
  | SetFontStyle TextStyle Scene   -- ^ Set the text style
  | SetFontColor Color Scene       -- ^ Set the text color

  | DrawLine MultiLine              -- ^ Draw a line of thickness 1.
                                -- Uses outline color by default.

  | DrawRectangle Float Float       -- ^ Rectangle with given width and height
  | DrawCircle Float                -- ^ Circle with the given radius
  | DrawSprite                      -- ^ Draw a sprite

  | SetFillColor Color Scene       -- ^ Set the fill color for shapes
  | SetOutlineColor Color Scene    -- ^ Set the color of a shape's outline
  | SetOutline Float Scene         -- ^ Set the thickness of a shape's outline

  | SetTexture Texture (Maybe (Rect Int)) Scene
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

drawLine :: Line Float -> Scene
drawLine = withLine \start end -> DrawLine (LineTo start (LineTo end LineEnd))

drawCircle :: Circle Float -> Scene
drawCircle c = Translate (circleCenter c) (DrawCircle (circleRadius c))

data MultiLine =
    LineColor Color MultiLine
  | LineTo {-# UNPACK #-} !(Vec Float) MultiLine
  | LineToRel {-# UNPACK #-} !(Vec Float) MultiLine
  | LineEnd