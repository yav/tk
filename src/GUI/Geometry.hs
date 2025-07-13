-- 2D Vectors and Rectangles
module GUI.Geometry (
  -- * Vectors
  Vec,
  Scalar(..),
  withVec',
  updVec, mapVec, zipVec,
  isUnitVec, len,
  dot, cross,

  -- * Shapes
  Line(..),
  lineIntersection,
  Rect(..),
) where

import Data.Vector.Unboxed qualified as V

-- | 2D Vectors
data family Vec scalar
data instance Vec Int     = VecInt !Int !Int
data instance Vec Float   = VecFloat !Float !Float


-- | A line segment
data Line a               = Line { lineStart :: !(Vec a), lineEnd :: !(Vec a) }
  deriving (Show,Read,Eq,Ord)

-- | A rectangle
data Rect a               = Rect { rectLoc :: !(Vec a), rectDim :: !(Vec a) }
  deriving (Show,Read,Eq,Ord)

data Poly a = Poly {
  polyCenter    :: !(Vec a),      -- ^ In world coordinates
  polyVertices  :: !(Vertices a)  -- ^ In local coordinates
}

-- XXX: poly constructor

-- `x` is even, `y` is odd
newtype Vertices a = Vertices (V.Vector a)


-- | Types that may be stored in a `Vec`.
class (Eq a, Ord a, Show a, Read a, Num a, V.Unbox a) => Scalar a where
  -- | Create a vector
  vec  :: a -> a -> Vec a

  -- | Do something with the components of a vector
  withVec :: (a -> a -> b) -> Vec a -> b

  -- | Convert to a `Float`
  toFloat :: a -> Float

instance Scalar a => Show (Vec a) where
  showsPrec n v = showsPrec n (withVec' v (,))

instance Scalar a => Read (Vec a) where
  readsPrec n s = [ (vec x y, r) | ((x,y),r) <- readsPrec n s ]

instance Scalar a => Eq (Vec a) where
  (==) = withVec \x1 y1 -> withVec \x2 y2 -> x1 == x2 && y1 == y2

-- | Lexicograhic ordering, first on `x`
instance Scalar a => Ord (Vec a) where
  compare = withVec \x1 y1 -> withVec \x2 y2 -> compare (x1,y1) (x2,y2)


-- | A version of `withVec` with swapped arguments
withVec' :: Scalar a => Vec a -> (a -> a -> b) -> b
withVec' = flip withVec
{-# inline withVec' #-}

-- | Update both coordinates
updVec :: (Scalar a, Scalar b) => (a -> b) -> (a -> b) -> Vec a -> Vec b
updVec f g = withVec \x y -> vec (f x) (g y)

-- | Apply a function to both coordinates of a vector
mapVec :: (Scalar a, Scalar b) => (a -> b) -> Vec a -> Vec b
mapVec f = updVec f f

-- | Combine two vectors pointwise
zipVec :: (Scalar a, Scalar b, Scalar c) => (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipVec f = withVec \x1 y1 -> withVec \x2 y2 -> vec (f x1 x2) (f y1 y2) 

-- | The square of the vector length
len2 :: Scalar a => Vec a -> a
len2 = withVec \x y -> x * x + y * y

-- | Is this a unit vector?
isUnitVec :: Scalar a => Vec a -> Bool
isUnitVec = (1 ==) . len2

-- | The length of a fector
len :: Scalar a => Vec a -> Float
len = sqrt . toFloat . len2

-- | Dot product
dot :: Scalar a => Vec a -> Vec a -> a
dot x y = withVec (+) (x * y)

-- | Cross products
cross :: Scalar a => Vec a -> Vec a -> a
cross = withVec \x1 y1 -> withVec \x2 y2 -> x1 * y2 - y1 * x2 


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
  vec = VecInt
  {-# inline vec #-}

  withVec k (VecInt x y) = k x y
  {-# inline withVec #-}

  toFloat = fromIntegral
  {-# inline toFloat #-}

instance Scalar Float where
  vec = VecFloat
  {-# inline vec #-}

  withVec k (VecFloat x y) = k x y
  {-# inline withVec #-}
  
  toFloat = id
  {-# inline toFloat #-}


-- | Compute the the interesection of two ilne segmentgs.
-- The results point to the intersection as an interpolant along each line:
-- 0 is at the start of the segment, 1 is at the end.
-- Negative or larger numbers indicate that the intersection is outside the segments.
-- If the lines don't intersect the result will be infinite or NaN.
lineIntersection :: Scalar a => Line a -> Line a -> (Float,Float)
lineIntersection
  Line { lineStart = a, lineEnd = b}
  Line { lineStart = c, lineEnd = d } = (t1, -t2)
  where
  ac = c - a
  ab = b - a
  cd = d - c
  t1 = toFloat (cross ac cd) / toFloat (cross ab cd)
  t2 = toFloat (cross ac ab) / toFloat (cross ab cd)


type Folder a s = (Int -> a -> s -> s) -> s -> V.Vector a -> s

-- | Iterate over the vertices of a polygon in world coordinates
foldVertices ::
  Scalar a =>
  Folder a s ->
  (Vec a -> s -> s) -> s -> Poly a -> s
foldVertices fold f s0 p = fold doV s0 vs
  where
  Vertices vs = polyVertices p
  doV i v s
    | even i =
        let !ve = polyCenter p + vec v (vs V.! (i+1))
        in f ve s
    | otherwise = s

-- | Iterate over the diagonals of a polygon, in world coordinates.
foldDiagonals :: Scalar a => Folder a s -> (Line a -> s -> s) -> s -> Poly a -> s
foldDiagonals fold f s0 p = foldVertices fold doD s0 p
  where
  doD v = f Line { lineStart = polyCenter p, lineEnd = v }

-- | Iterate over the edges of apolygon in world coordinates
foldEdges ::
  Scalar a =>
  Folder a (Maybe (Vec a, Vec a, s)) ->
  (Line a -> s -> s) -> s -> Poly a -> s
foldEdges fold f s0 p =
  case foldVertices fold doE Nothing p of
    Just (v0, v, s) -> step v v0 s
    Nothing -> s0 
  where
  step prev v = f Line { lineStart = prev, lineEnd = v}
  doE v s =
    case s of
      Nothing -> Just (v, v, s0)
      Just (v0, prev, st) ->
        let !st1 = step prev v st
        in Just (v0, v, st1) 

lazy :: Scalar a => Folder a s
lazy = V.ifoldr

strict :: Scalar a => Folder a s
strict f = V.ifoldl' (\s i a -> f i a s)

polyOverlaps :: Scalar a => Poly a -> Poly a -> Bool
polyOverlaps p1' p2' = overlaps p1' p2' || overlaps p2' p1'
  where
  overlaps p1 p2   = foldDiagonals lazy (checkD p2) False p1
  checkD p2 d next = foldDiagonals lazy (checkDE d) next p2
  checkDE d e next =
    case lineIntersection d e of
      (t, _) -> 0 <= t && t <= 1 || next