-- 2D Vectors and Rectangles
module GUI.Geometry (
  -- * Vectors
  Vec,
  Scalar,
  vec,
  getX, getY,
  setX, setY,
  updX, updY,
  updVec, mapVec, zipVec,
  len2, isUnitVec, len,

  -- * Rectangles
  Rect,
  rect,
  getRectLoc, getRectDim,
  setRectLoc, setRectDim,
  updRectLoc, updRectDim,
  updRect
) where

-- | 2D Vectors
data family Vec scalar
data instance Vec Int     = VecI { iX :: !Int, iY :: !Int }
data instance Vec Float   = VecF { fX :: !Float, fY :: !Float }

data family Rect a
data instance Rect Int    = RectI { iLoc :: !(Vec Int), iDim :: !(Vec Int) }
data instance Rect Float  = RectF { fLoc :: !(Vec Float), fDim :: !(Vec Float) }


-- | Types that may be stored in a `Vec`.
class (Eq a, Num a) => Scalar a where
  -- | Create a vector
  vec  :: a -> a -> Vec a

  -- | Get the `x` coorindate
  getX :: Vec a -> a

  -- | get the `y` coordinate
  getY :: Vec a -> a

  -- | Create a rectangle
  rect    :: Vec a {-^ Location -} -> Vec a {-^ Dimensions -} -> Rect a

  -- | Get the location of a rectangle
  getRectLoc :: Rect a -> Vec a

  -- | Get the dimensions of the rectangle
  getRectDim :: Rect a -> Vec a

  toFloat :: a -> Float

-- | Set the `x` coordinate
setX :: Scalar a => a -> Vec a -> Vec a
setX x v = vec x (getY v)

-- | Set the `y` coordinate
setY :: Scalar a => a -> Vec a -> Vec a
setY y v = vec (getX v) y

-- | Update the `x` coordinate
updX :: Scalar a => (a -> a) -> Vec a -> Vec a
updX f v = setX (f (getX v)) v

-- | Update the `y` coordinate
updY :: Scalar a => (a -> a) -> Vec a -> Vec a
updY f v = setY (f (getY v)) v

-- | Update both coordinates
updVec :: (Scalar a, Scalar b) => (a -> b) -> (a -> b) -> Vec a -> Vec b
updVec x y v = vec (x (getX v)) (y (getY v))

-- | Apply a function to both coordinates of a vector
mapVec :: (Scalar a, Scalar b) => (a -> b) -> Vec a -> Vec b
mapVec f = updVec f f

-- | Combine two vectors pointwise
zipVec :: (Scalar a, Scalar b, Scalar c) => (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipVec f x y = vec (f (getX x) (getX y)) (f (getY x) (getY y))

-- | The square of the vector length
len2 :: Scalar a => Vec a -> a
len2 v = x * x + y * y
  where x = getX v
        y = getY v

-- | Is this a unit vector?
isUnitVec :: Scalar a => Vec a -> Bool
isUnitVec = (1 ==) . len2

-- | The length of a fector
len :: Vec Float -> Float
len = sqrt . toFloat . len2


instance Scalar a  => Num (Vec a) where
  (+) = zipVec (+)
  (-) = zipVec (-)
  (*) = zipVec (*)
  negate = mapVec negate
  fromInteger x = vec y y
    where y = fromInteger x
  abs = error "Vec: abs"
  signum = error "Vec: signum" 

instance Scalar Int where
  vec = VecI
  getX = iX
  getY = iY
  rect = RectI
  getRectLoc = iLoc
  getRectDim = iDim
  toFloat = fromIntegral

instance Scalar Float where
  vec = VecF
  getX = fX
  getY = fY
  rect = RectF
  getRectLoc = fLoc
  getRectDim = fDim
  toFloat = id

-- | Set the location of a rectangle
setRectLoc :: Scalar a => Vec a -> Rect a -> Rect a
setRectLoc x v = rect x (getRectDim v)

-- | Set the dimensions of a rectangle
setRectDim :: Scalar a => Vec a -> Rect a -> Rect a
setRectDim y v = rect (getRectLoc v) y

-- | Update the location of a rectangle
updRectLoc :: Scalar a => (Vec a -> Vec a) -> Rect a -> Rect a
updRectLoc f v = setRectLoc (f (getRectLoc v)) v

-- | Update the dimensions of a rectangle
updRectDim :: Scalar a => (Vec a -> Vec a) -> Rect a -> Rect a
updRectDim f v = setRectDim (f (getRectDim v)) v

-- | Update both the location and dimensions of a recantgle
updRect :: (Scalar a, Scalar b) => (Vec a -> Vec b) -> (Vec a -> Vec b) -> Rect a -> Rect b
updRect x y v = rect (x (getRectLoc v)) (y (getRectDim v))