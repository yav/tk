module GUI (
  gui, App(..),
  SFML.SFEvent(..),
  SFML.Font,
  SFML.Texture,
  module Export
) where

import Control.Monad
import Control.Exception
import SFML.Window qualified as SFML
import SFML.Graphics qualified as SFML
import SFML.Graphics.Color as Export

import GUI.ResourcePool
import GUI.Scene

data RO s = RO {
  roApp  :: App s,
  roWin  :: SFML.RenderWindow,
  roFont :: SFML.Font 
}

data App s = App {
  appTitle :: String,
  appFrameRate :: Int,
  appInit :: s,
  appEvent :: SFML.SFEvent -> s -> s,
  appUpdate :: s -> Maybe s,
  appDraw :: s -> Scene,
  appFont :: FilePath
}



gui :: App s -> IO ()
gui app =
  do
    wmode <- SFML.getDesktopMode 
    w <- SFML.createRenderWindow wmode  { SFML.windowWidth = 800, SFML.windowHeight = 600 } (appTitle app) [SFML.SFDefaultStyle] Nothing
    do
      SFML.setFramerateLimit w (appFrameRate app)
      fo <- SFML.err (SFML.fontFromFile (appFont app))
      let ro = RO { roWin = w, roApp = app, roFont = fo }
      rsr <- loop ro noResources (appInit app) `finally` SFML.destroy w
      destroyResources rsr
     `finally` SFML.destroy w



getEvents :: RO s -> s -> IO s
getEvents ro s =
  do
    mb <- SFML.pollEvent (roWin ro)
    case mb of
      Nothing -> pure s
      Just ev ->
        getEvents ro $! appEvent (roApp ro) ev s


loop :: RO s -> Resources -> s -> IO Resources
loop ro rs us =
  do
    us1 <- getEvents ro us
    let app = roApp ro
    case appUpdate app us1 of
      Nothing -> pure rs
      Just s ->
        do
          let w = roWin ro
          rs1 <- renderScene w (roFont ro) (appDraw app s) rs
          SFML.display w
          loop ro rs1 s