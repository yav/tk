module Main where

import Rule2D
import Transform qualified as T
import Vec2D
import GUI
import GUI.Event
import GUI.Scene

main :: IO ()
main = gui App {
  appTitle = "TK",
  appFrameRate = 60,
  appInit = initS,
  appUpdate = updateS,
  appEvent = handleEvent,
  appDraw = drawS
}


data S = S {
  blockS :: Block,
  done  :: Bool
} deriving Show

initS :: S
initS =
  case parseBlock b of
    Right a -> S { blockS = a, done = False }
    Left err -> error err
  where
  b = unlines
    [ "1..2 ...."
    , ".... 1..."
    , ".... 1... ...."
    ]

drawS :: S -> Scene
drawS b = drawBlock (blockS b)

handleEvent :: SFEvent -> S -> S
handleEvent ev s@S { blockS = b } =
  case ev of
    SFEvtClosed -> s { done = True }
    SFEvtKeyReleased { code = KeyX } -> s { blockS = transform T.flipX b }
    SFEvtKeyReleased { code = KeyY } -> s { blockS = transform T.flipY b }
    SFEvtKeyReleased { code = KeyE } -> s { blockS = transform (T.rotate 1) b }
    SFEvtKeyReleased { code = KeyQ } -> s { blockS = transform (T.rotate (-1)) b }
    SFEvtKeyReleased { code = KeyW } -> s { blockS = moveBy (Vec2D 0 (-1)) b }
    SFEvtKeyReleased { code = KeyA } -> s { blockS = moveBy (Vec2D (-1) 0) b }
    SFEvtKeyReleased { code = KeyS } -> s { blockS = moveBy (Vec2D 0 1) b }
    SFEvtKeyReleased { code = KeyD } -> s { blockS = moveBy (Vec2D 1 0) b }
    _ -> s


updateS :: S -> Maybe S
updateS s = if done s then Nothing else Just s
