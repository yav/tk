module Rule2D (
  Block,
  block,
  parseBlock,
  drawBlock,
  positions,
  moveBy,
  transform
) where

import Control.Monad(zipWithM)
import GUI
import Vec2D
import Transform qualified as T
import Rule

data Block = Block {
  dim   :: Vec2D Int,
  pos   :: Vec2D Int,
  tr    :: T.Transform,
  locs  :: [(Vec2D Int,Rule)]
} deriving Show

block :: [(Vec2D Int,Rule)] -> Block
block as = Block { pos = 0, tr = mempty, locs = as, dim = foldl' upd 0 as }
  where
  upd m (p,_) = vop2 max m (p + 1)

positions :: Block -> [(Vec2D Int,Rule)]
positions b = [ (pos b + T.transform (tr b) (dim b) p, a) | (p,a) <- locs b ]

moveBy :: Vec2D Int -> Block -> Block
moveBy v b = b { pos = pos b +  v }

transform :: T.Transform -> Block -> Block
transform t b = b { tr = t <> tr b }  

drawBlock :: Block -> Scene
drawBlock b =
    foldr (:&:) Blank
    [ Translate (cvt x) (cvt y) (drawRule sz a) | (Vec2D x y,a) <- positions b ]
  where
  sz = 32
  cvt x = sz * fromIntegral x  

parseBlock :: String -> Either String Block
parseBlock = fmap (block . concat . concat) . mapM parseLine . zip [0..] . lines
  where
  actLen = 5 -- 1 for space
  parseLine (row, cs) = zipWithM getA [0 ..] (chunk actLen cs) 
    where
    getA col xs =
      case parseRule (take (actLen-1) xs) of
        Just a  -> pure [(Vec2D col row, a) | not (isTrivial a) ]
        Nothing ->
          Left ("Malformed action " ++ show (row+1) ++ ":" ++ show (1 + actLen * col) ++ ": " ++ xs)

chunk :: Int -> [a] -> [[a]]
chunk n xs =
  case xs of
    [] -> []
    _  ->
      case splitAt n xs of
        (as,bs) -> as : chunk n bs

