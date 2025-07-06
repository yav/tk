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
import GUI.Scene
import GUI.Render
import GUI.Timer

data RO s = RO {
  roApp  :: App s,
  roWin  :: SFML.RenderWindow,
  roFont :: SFML.Font,
  roClock :: SFML.Clock
}

data App s = App {
  appTitle :: String,
  appFrameRate :: Int,
  appInit :: Time -> s,
  appEvent :: SFML.SFEvent -> Time -> s -> s,
  appUpdate :: Time -> s -> Maybe s,
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
      now <- milliSeconds . SFML.asMilliseconds <$> SFML.getElapsedTime clock
      destroyResources =<< loop ro noTimers noResources (appInit app now)
  

withResource :: SFML.SFResource r => IO r -> (r -> IO ()) -> IO ()
withResource mk k =
  do r <- mk
     k r `finally` SFML.destroy r


getEvents :: RO s -> Time -> s -> IO s
getEvents ro t s =
  do
    mb <- SFML.pollEvent (roWin ro)
    case mb of
      Nothing -> pure s
      Just ev ->
        getEvents ro t $! appEvent (roApp ro) ev t s


loop :: RO s -> Timers a -> Resources -> s -> IO Resources
loop ro ts rs us =
  do
    t <- milliSeconds . SFML.asMilliseconds <$> SFML.getElapsedTime (roClock ro)
    us1 <- getEvents ro t us
    let app = roApp ro
    case appUpdate (roApp ro) t us1 of
      Nothing -> pure rs
      Just s ->
        do
          let w = roWin ro
          rs1 <- renderScene w (roFont ro) (appDraw app s) rs
          SFML.display w
          let ts1 = ts
          loop ro ts1 rs1 s