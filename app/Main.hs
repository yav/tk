module Main where

import Rule2D
import Transform qualified as T
import Vec2D
import GUI
import GUI.Event
import GUI.Scene
import GUI.Timer

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
  blockS  :: Block,
  done    :: Bool,
  timers  :: Timers (S -> S),
  counter :: !Int,
  perCounter :: !Int
}

initS :: Time -> S
initS now =
  case parseBlock b of
    Right a -> S { blockS = a, done = False, timers = timer now True 1000 upd noTimers, counter = 0, perCounter = 0 }
    Left err -> error err
  where
  upd s = s { perCounter = perCounter s + 1 }
  b = unlines
    [ "1..2 ...."
    , ".... 1..."
    , ".... 1... ...."
    ]

drawS :: S -> Scene
drawS b = Text (show (perCounter b, counter b)) :&: drawBlock (blockS b)

handleEvent :: SFEvent -> Time -> S -> S
handleEvent ev now s@S { blockS = b } =
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
    SFEvtKeyReleased { code = KeyT } -> s { timers = timer now False 1000 (\st -> st { counter = counter st - 1 }) (timers s), counter = counter s + 1 }
    _ -> s


updateS :: Time -> S -> Maybe S
updateS now s
  | done s = Nothing
  | otherwise = Just $!
    case updateTimers now (timers s) of
      ([],_) -> s
      (fs,ts1) -> foldr ($) s { timers = ts1 } fs