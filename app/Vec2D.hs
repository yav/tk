module Vec2D where

data Vec2D a = Vec2D a a
  deriving (Show,Eq,Ord)

instance Functor Vec2D where
  fmap f (Vec2D x y) = Vec2D (f x) (f y)

vop2 :: (a -> b -> c) -> Vec2D a -> Vec2D b -> Vec2D c
vop2 f (Vec2D x y) (Vec2D a b) = Vec2D (f x a) (f y b)

vzip :: Vec2D a -> Vec2D b -> Vec2D (a,b)
vzip (Vec2D x y) (Vec2D a b) =Vec2D (x,a) (y,b)

swap :: Vec2D a -> Vec2D a
swap (Vec2D x y) = Vec2D y x

-- Pointwise
instance Num a => Num (Vec2D a) where
  (+) = vop2 (+)
  (-) = vop2 (-)
  (*) = vop2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger x = Vec2D y y
    where y = fromInteger x
