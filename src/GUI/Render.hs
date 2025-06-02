module GUI.Render where

import SFML.System.Vector2 qualified as SFML
import SFML.Graphics qualified as SFML

import GUI.ResourcePool
import GUI.Scene

data TextProps = TextProps {
  txtColor :: SFML.Color,
  txtFont  :: SFML.Font,
  txtStyle :: [SFML.TextStyle],
  txtSize  :: Int
}

defaultTextProps :: SFML.Font -> TextProps
defaultTextProps fnt = TextProps {
  txtColor = SFML.white,
  txtFont  = fnt,
  txtStyle = [],
  txtSize  = 16
}

data ShapeProps = ShapeProps {
  fillColor :: SFML.Color,
  outlineColor :: SFML.Color,
  outlineThikness :: Float 
}

defaultShapeProps :: ShapeProps
defaultShapeProps = ShapeProps {
  fillColor = SFML.black,
  outlineColor = SFML.white,
  outlineThikness = 0
}

renderScene :: SFML.RenderWindow -> SFML.Font -> Scene -> Resources -> IO Resources
renderScene w fnt scn rsr =
  do SFML.clearRenderWindow w SFML.black
     rs <- renderLoop w (defaultTextProps fnt) defaultShapeProps SFML.idTransform rsr scn
     resetResources rs

setShapeProps :: SFML.SFShape a => a -> ShapeProps -> IO ()
setShapeProps obj sh =
  do
    SFML.setFillColor obj (fillColor sh)
    SFML.setOutlineColor obj (outlineColor sh)
    SFML.setOutlineThickness obj (outlineThikness sh)


renderLoop :: SFML.RenderWindow -> TextProps -> ShapeProps -> SFML.Transform -> Resources -> Scene -> IO Resources
renderLoop w txt sh trans rsr scn =
  case scn of
    Blank -> pure rsr
    x :&: y ->
      do rsr1 <- renderLoop w txt sh trans rsr x
         renderLoop w txt sh trans rsr1 y

    Font fo k -> renderLoop w txt { txtFont = fo } sh trans rsr k
    FontSize n k -> renderLoop w txt { txtSize = n } sh trans rsr k
    FontStyle s k -> renderLoop w txt { txtStyle = s : txtStyle txt } sh trans rsr k
    FontColor c k -> renderLoop w txt { txtColor = c } sh trans rsr k

    FillColor c k -> renderLoop w txt sh { fillColor = c } trans rsr k
    OutlineColor c k -> renderLoop w txt sh { outlineColor = c } trans rsr k
    Outline n k -> renderLoop w txt sh { outlineThikness = n } trans rsr k

    Text str ->
      do (obj, rsr1) <- getResource rsr
         SFML.setTextStringU obj str
         SFML.setTextFont obj (txtFont txt)
         SFML.setTextCharacterSize obj (txtSize txt)
         SFML.setTextStyle obj (txtStyle txt)
         SFML.setTextColor obj (txtColor txt)
         SFML.drawText w obj (Just SFML.renderStates { SFML.transform = trans })
         pure rsr1

    Rectangle width height ->
      do
        (obj, rsr1) <- getResource rsr
        SFML.setSize obj (SFML.Vec2f width height)
        setShapeProps obj sh
        SFML.drawRectangle w obj (Just SFML.renderStates { SFML.transform = trans })
        pure rsr1

    Circle radius ->
      do
        (obj, rsr1) <- getResource rsr
        SFML.setRadius obj radius
        setShapeProps obj sh
        SFML.drawCircle w obj (Just SFML.renderStates { SFML.transform = trans })
        pure rsr1

    Translate dx dy k -> renderLoop w txt sh (SFML.translation dx dy * trans) rsr k
    Scale sx sy k     -> renderLoop w txt sh (SFML.scaling sx sy * trans) rsr k
    ScaleWithCenter sx sy x y k -> renderLoop w txt sh (SFML.scalingWithCenter sx sy x y * trans) rsr k
    Rotate r k -> renderLoop w txt sh (SFML.rotation r * trans) rsr k
    RotateWithCenter r x y k -> renderLoop w txt sh (SFML.rotationWithCenter r x y * trans) rsr k