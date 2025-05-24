module Transform (
  Transform,
  rotate,
  flipX,
  flipY,
  inv,
  transform
) where

import Vec2D

data Transform = Transform {
  flipped :: Bool,
  rot     :: Int    -- mod 4
} deriving Show

instance Semigroup Transform where
  x <> y = Transform {
    rot     = (rot x + rot y) `mod` 4,
    flipped = flipped x /= flipped y
  }

instance Monoid Transform where
  mempty = Transform { rot = 0, flipped = False }


-- | Rottate by 90*n degrees counter-clockwise
rotate :: Int -> Transform
rotate n = Transform { flipped = False, rot = n `mod` 4 }

-- | Flip around X axis
flipX :: Transform
flipX = Transform { flipped = True, rot = 0 }

-- | Flip around Y axis 
flipY :: Transform
flipY = rotate 2 <> flipX

-- | Inverse transform
inv :: Transform -> Transform
inv t = t { rot = (4 - rot t) `mod` 4 }


data Trans2 = Trans2 {
  fX :: Bool, fY :: Bool, tr :: Bool
}

toTrans2 :: Transform -> Trans2
toTrans2 t = if tr r then r { fY = flipped t /= fY r }
                     else r { fX = flipped t /= fX r }
  where
  z = Trans2 { fX = False, fY = False, tr = False }
  r = case rot t of
        0 -> z
        1 -> z { tr = True, fY = True }
        2 -> z { fX = True, fY = True }
        _ -> z { tr = True, fX = True }

-- | Apply a transformation
transform :: Transform -> Vec2D Int {- ^ dimensions -} -> Vec2D Int -> Vec2D Int
transform t dim pt = tt
  where
  t2            = toTrans2 t
  pt1           = flp <$> (Vec2D (fX t2) (fY t2) `vzip` dim `vzip` pt)
  tt            = if tr t2 then swap pt1 else pt1
  flp ((f,d),p) = if f then d - p - 1 else p