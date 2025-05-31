module GUI.Scene where

import SFML.Graphics.RenderWindow qualified as SFML
import SFML.Graphics.Types qualified as SFML
import SFML.Graphics.Color qualified as SFML
import SFML.Graphics.Text qualified as SFML
import SFML.Graphics.Transform qualified as SFML
import SFML.Graphics.RenderStates qualified as SFML

import GUI.ResourcePool

data Scene =
    Text String
  | Font SFML.Font Scene
  | FontSize Int Scene
  | FontStyle SFML.TextStyle Scene
  | FontColor SFML.Color Scene
  | Translate Float Float Scene


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


renderScene :: SFML.RenderWindow -> SFML.Font -> Scene -> Resources -> IO Resources
renderScene w fnt scn rsr =
  do SFML.clearRenderWindow w SFML.black
     rs <- renderLoop w (defaultTextProps fnt) SFML.idTransform rsr scn
     resetResources rs

renderLoop :: SFML.RenderWindow -> TextProps -> SFML.Transform -> Resources -> Scene -> IO Resources
renderLoop w txt trans rsr scn =
  case scn of
    Font fo k -> renderLoop w txt { txtFont = fo } trans rsr k
    FontSize n k -> renderLoop w txt { txtSize = n } trans rsr k
    FontStyle s k -> renderLoop w txt { txtStyle = s : txtStyle txt } trans rsr k
    FontColor c k -> renderLoop w txt { txtColor = c } trans rsr k
    Translate dx dy k -> renderLoop w txt (SFML.translation dx dy * trans) rsr k
    Text str ->
      do (obj, rsr1) <- getResource rsr
         SFML.setTextStringU obj str
         SFML.setTextFont obj (txtFont txt)
         SFML.setTextCharacterSize obj (txtSize txt)
         SFML.setTextStyle obj (txtStyle txt)
         SFML.setTextColor obj (txtColor txt)
         SFML.drawText w obj (Just SFML.renderStates { SFML.transform = trans })
         pure rsr1
