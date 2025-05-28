module Main where

import Graphics.Gloss.Interface.Pure.Game
import Rule2D
import Transform qualified as T
import Vec2D
import GUI


main :: IO ()
main = gui App {
  appTitle = "TK",
  appFrameRate = 60,
  appInit = False,
  appUpdate = \s -> pure s,
  appEvent = \ev _s ->
    case ev of
      SFEvtClosed -> pure True
      _ -> pure False,
  appFinished = \s -> pure s 
}

{-
-- print initS >> play display bgColor fps initS drawS handleEvent updateS

display :: Display

display = InWindow "TK" (800,600) (0,0)

bgColor :: Color
bgColor = black

fps :: Int
fps = 60

type S = Block

initS :: S
initS =
  case parseBlock b of
    Right a -> a
    Left err -> error err
  where
  b = unlines
    [ "1..2 ...."
    , ".... 1..."
    , ".... 1... ...."
    ]

drawS :: S -> Picture
drawS b = pictures
  [ drawBlock b
  , translate (-300) 200 $ scale 0.1 0.1 $ color white (text (show b)) 
  ]
  
handleEvent :: Event -> S -> S
handleEvent ev s =
  case ev of
    EventKey (Char 'x') Up _ _ -> transform T.flipX s
    EventKey (Char 'y') Up _ _ -> transform T.flipY s
    EventKey (Char 'e') Up _ _ -> transform (T.rotate (-1)) s
    EventKey (Char 'q') Up _ _ -> transform (T.rotate 1) s
    EventKey (Char 'w') Up _ _ -> moveBy (Vec2D 0 1) s
    EventKey (Char 'a') Up _ _ -> moveBy (Vec2D (-1) 0) s
    EventKey (Char 's') Up _ _ -> moveBy (Vec2D 0 (-1)) s
    EventKey (Char 'd') Up _ _ -> moveBy (Vec2D 1 0) s
    _ -> s

updateS :: Float -> S -> S
updateS _ s = s
-}