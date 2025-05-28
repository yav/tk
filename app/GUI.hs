module GUI (
  gui, App(..), SFML.SFEvent(..)
) where

import Control.Monad
import Control.Exception
import Data.Map(Map)
import Data.Map qualified as Map
import SFML.Window qualified as SFML
import SFML.Graphics qualified as SFML


newtype M s a = M (App s -> GUIState -> s -> IO (a, GUIState, s)) 

instance Functor (M s) where
  fmap = liftM

instance Applicative (M s) where
  pure a = M \_ s1 s2 -> pure (a,s1,s2)
  (<*>) = ap

instance Monad (M s) where
  M m >>= k = M \ro gs s ->
    do (a, gs1, s1) <- m ro gs s
       let M m1 = k a
       m1 ro gs1 s1


data App s = App {
  appTitle :: String,
  appFrameRate :: Int,
  appInit :: s,
  appEvent :: SFML.SFEvent -> s -> IO s,
  appUpdate :: s -> IO s,
  appFinished :: s -> IO Bool
}

data GUIState = GUIState {
  guiWindow :: SFML.RenderWindow,
  guiTextObjects :: ([SFML.Text],[SFML.Text]),
  guiFonts :: Map FilePath SFML.Font
}




gui :: App s -> IO ()
gui app =
  do
    wmode <- SFML.getDesktopMode 
    w <- SFML.createRenderWindow wmode  { SFML.windowWidth = 800, SFML.windowHeight = 600 } (appTitle app) [SFML.SFDefaultStyle] Nothing
    SFML.setFramerateLimit w (appFrameRate app)
    let gs = GUIState { guiWindow = w, guiTextObjects = ([],[]), guiFonts = mempty }
    let M m = loop
    _ <- m app gs (appInit app) `finally` SFML.destroy w
    pure ()

io :: IO a -> M s a
io i = M \_ s us ->
  do a <- i
     pure (a, s, us)

guiS :: M s GUIState
guiS = M \_ s us -> pure (s,s,us)

userS :: M s s
userS = M \_ s us -> pure (us,s,us)

setGuiS :: GUIState -> M s ()
setGuiS s = M \_ _ us -> pure ((), s, us)

setUS :: s -> M s ()
setUS us = M \_ s _ -> pure ((), s, us)

getApp :: M s (App s)
getApp = M \r s us -> pure (r, s, us)

getFont :: FilePath -> M s SFML.Font
getFont f =
  do
    s <- guiS
    case Map.lookup f (guiFonts s) of
      Just fo -> pure fo
      Nothing ->
        do
          fo <- io (SFML.err (SFML.fontFromFile f))
          setGuiS s { guiFonts = Map.insert f fo (guiFonts s) }
          pure fo

getText :: M s SFML.Text
getText =
  do
    s <- guiS
    case guiTextObjects s of
      (x:xs,used) -> setGuiS s { guiTextObjects = (xs,x:used) } >> pure x
      ([], used) ->
        do
          txt <- io (SFML.err SFML.createText)
          setGuiS s { guiTextObjects = ([], txt:used) }
          pure txt

reset :: M s ()
reset =
  do
    s <- guiS
    case guiTextObjects s of
      (as,bs) -> setGuiS s { guiTextObjects = (as ++ bs, []) }


drawText :: SFML.Font -> String -> M s SFML.Text
drawText f s =
  do txt <- getText
     io do
           SFML.setTextFont txt f
           SFML.setTextStringU txt s
           SFML.setTextColor txt SFML.green
           SFML.setTextCharacterSize txt 32
           SFML.setTextStyle txt []
           pure txt


getEvents :: M s ()
getEvents =
  do
    w  <- guiWindow <$> guiS
    mb <- io (SFML.pollEvent w)
    case mb of
      Nothing -> pure ()
      Just ev ->
        do s <- userS
           a <- getApp
           setUS =<< io (appEvent a ev s)
           getEvents


draw :: M s ()
draw = 
  do
    reset
    s <- guiS
    let w = guiWindow s
    io (SFML.clearRenderWindow w SFML.black)
    fo <- getFont "resource/font/Modak-Regular.ttf"
    txt <- drawText fo "Hello"
    io (SFML.drawText w txt Nothing)


loop :: M s ()
loop =
  do
    getEvents
    app <- getApp
    s   <- userS
    done <- io (appFinished app s)
    unless done 
      do
        draw
        w <- guiWindow <$> guiS
        io (SFML.display w)
        loop