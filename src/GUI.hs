module GUI (
  gui, App(..)
) where

import Data.ByteString(ByteString)
import Data.ByteString.Unsafe qualified as BSU
import Data.FileEmbed(embedFileRelative)
import Control.Exception
import Foreign.Ptr(castPtr)
import SFML.Window qualified as SFML
import SFML.Graphics qualified as SFML
import SFML.SFResource qualified as SFML


import GUI.ResourcePool
import GUI.Timer
import GUI.Scene
import GUI.Render

data RO s = RO {
  roApp  :: App s,
  roWin  :: SFML.RenderWindow,
  roFont :: SFML.Font,
  roClock :: SFML.Clock
}

data App s = App {
  appTitle :: String,
  appFrameRate :: Int,
  appInit :: s,
  appEvent :: SFML.SFEvent -> s -> s,
  appUpdate :: Int -> s -> Maybe s,
  appDraw :: s -> Scene
}

defaultFontData :: ByteString
defaultFontData = $(embedFileRelative "resource/font/default.ttf")


gui :: App s -> IO ()
gui app =
  SFML.getDesktopMode >>= \wmode ->
  withResource (SFML.createRenderWindow wmode  { SFML.windowWidth = 800, SFML.windowHeight = 600 } (appTitle app) [SFML.SFDefaultStyle] Nothing) \w ->
  withResource SFML.createClock \clock ->
    do
      SFML.setFramerateLimit w (appFrameRate app)
      fo <- BSU.unsafeUseAsCStringLen defaultFontData \(ptr,len) ->
              SFML.err (SFML.fontFromMemory (castPtr ptr) len)
      let ro = RO { roWin = w, roApp = app, roFont = fo, roClock = clock }
      t0 <- SFML.getElapsedTime clock
      destroyResources =<< loop t0 ro noTimers noResources (appInit app)
  

withResource :: SFML.SFResource r => IO r -> (r -> IO ()) -> IO ()
withResource mk k =
  do r <- mk
     k r `finally` SFML.destroy r


getEvents :: RO s -> s -> IO s
getEvents ro s =
  do
    mb <- SFML.pollEvent (roWin ro)
    case mb of
      Nothing -> pure s
      Just ev ->
        getEvents ro $! appEvent (roApp ro) ev s


loop :: SFML.Time -> RO s -> Timers a -> Resources -> s -> IO Resources
loop prev ro ts rs us =
  do
    us1 <- getEvents ro us
    let app = roApp ro
    t <- SFML.getElapsedTime (roClock ro)
    let delta = t - prev
    case appUpdate app (SFML.asMilliseconds delta) us1 of
      Nothing -> pure rs
      Just s ->
        do
          let w = roWin ro
          rs1 <- renderScene w (roFont ro) (appDraw app s) rs
          SFML.display w
          let ts1 = ts
          loop t ro ts1 rs1 s