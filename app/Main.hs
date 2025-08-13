module Main where

import Rule2D
import Transform qualified as T
import Vec2D
import GUI
import GUI.Event
import GUI.Scene
import GUI.Timer
import GUI.Geometry
import GUI.Color

main :: IO ()
main = gui App {
  appTitle = "TK",
  appFrameRate = Nothing, -- Just 60,
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
    Right a -> S { blockS = a, done = False, timers = timer now True 10 upd noTimers, counter = 0, perCounter = 0 }
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
          :&: Translate (vec 200 200)  (Rotate (45 * fromIntegral (perCounter b) / 10) pic1)
  where
  l1 = lineFromTo (vec (0 :: Float) 0) (vec 50 50)
  l2 = lineFromTo (vec 50 0) (vec 0 50)
  Just t1 = lineIntersection l1 l2
  pt = lineStart l1 + t1 .* lineDir l1
  r = 5
  obj c = Translate ((-1) .* vec r r) $ FillColor c (Circle r)
  pic1 = obj blue :&: Translate pt (obj yellow) :&: OutlineColor red (line l1) :&: OutlineColor green (line l2)


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