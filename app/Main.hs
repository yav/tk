module Main where

import Graphics.Gloss.Interface.Pure.Game
import Basics

main :: IO ()
main = play display bgColor fps initS drawS handleEvent updateS

display :: Display

display = InWindow "TK" (800,600) (0,0)

bgColor :: Color
bgColor = black

fps :: Int
fps = 60

type S = Transform

initS :: S
initS = Basics.rotate 1

drawS :: S -> Picture
drawS t =
  translate (-totW/2) (totH/2) $ 
  pictures [ rect (x,y,c) | x <- take w [ 0 .. ], y <- take h [ 0 .. ],
             let c' = lin red blue x (w-1), let c = lin c' green y (h-1) ]
  where
  (w',h') = if rot t == 1 || rot t == 3 then (h,w) else (w,h)
  totW = fromIntegral w' * size
  totH = fromIntegral h' * size
  (w,h) = (5,10)
  lin a b n m =
    let (r1,g1,b1,a1) = rgbaOfColor a
        (r2,g2,b2,a2) = rgbaOfColor b
        x             = fromIntegral n / fromIntegral m
    in makeColor (r1 + x * r2) (g1 + x * g2) (b1 + x * b2) (a1 + x * a2) 
  size = 32
  rect (x',y',c) =
    let (x,y) = trans t (w,h) (x',y') in
    color c $
    translate (fromIntegral x * size + 1) (- (fromIntegral y * size + 1)) $
    rectangleSolid (size - 2) (size - 2)

handleEvent :: Event -> S -> S
handleEvent ev s =
  case ev of
    EventKey (Char 'x') Up _ _ -> flipX <> s
    EventKey (Char 'y') Up _ _ -> flipY <> s
    EventKey (Char 'r') Up _ _ -> Basics.rotate 1 <> s
    _ -> s

updateS :: Float -> S -> S
updateS _ s = s
