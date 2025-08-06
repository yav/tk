-- 2D Vectors and Rectangles
module GUI.Geometry (
  -- * Vectors
  Vec,
  Scalar(..),
  withVec',
  updVec, mapVec, zipVec,
  isUnitVec, len,
  dot, normal,

  -- * Shapes
  Line(..),
  lineFromTo,
  lineIntersection,
  Rect(..),
  Polygon,
  listPolygon,
  funPolygon,
  polyVertexNum,
  polyVertex,
  polyEdge,
) where

import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as MV

-- | 2D Vectors
data family Vec scalar
data instance Vec Int     = VecInt !Int !Int
data instance Vec Float   = VecFloat !Float !Float


-- | A line segment
data Line a = Line {
  lineLoc :: !(Vec a),  --  ^ Starting point of line segment
  lineDir :: !(Vec a)   -- ^ Length and direction of line
} deriving (Show,Read,Eq,Ord)

-- | A rectangle
data Rect a               = Rect { rectLoc :: !(Vec a), rectDim :: !(Vec a) }
  deriving (Show,Read,Eq,Ord)

-- | A polygon
newtype Polygon a         = Polygon (V.Vector a)


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
{-# inline updVec #-}

-- | Apply a function to both coordinates of a vector
mapVec :: (Scalar a, Scalar b) => (a -> b) -> Vec a -> Vec b
mapVec f = updVec f f
{-# inline mapVec #-}

-- | Combine two vectors pointwise
zipVec :: (Scalar a, Scalar b, Scalar c) => (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipVec f = withVec \x1 y1 -> withVec \x2 y2 -> vec (f x1 x2) (f y1 y2) 
{-# inline zipVec #-}

-- | The square of the vector length
len2 :: Scalar a => Vec a -> a
len2 = withVec \x y -> x * x + y * y
{-# inline len2 #-}

-- | Is this a unit vector?
isUnitVec :: Scalar a => Vec a -> Bool
isUnitVec = (1 ==) . len2
{-# inline isUnitVec #-}


-- | The length of a fector
len :: Scalar a => Vec a -> Float
len = sqrt . toFloat . len2
{-# inline len #-}

-- | Dot product
dot :: Scalar a => Vec a -> Vec a -> a
dot x y = withVec (+) (x * y)
{-# inline dot #-}



-- | Rotate a vector 90 degrees counter-clockwise
-- (i.e., from `x` toward `y` axis)
normal :: Scalar a => Vec a -> Vec a
normal = withVec \x y -> vec (-y) x
{-# inline normal #-}


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


-- | Compute the the interesection of two line segmentgs.
-- The results point to the intersection as an interpolant along each line:
-- 0 is at the start of the segment, 1 is at the end.
-- Negative or larger numbers indicate that the intersection is outside the segments.
-- If the results are `NaN`, then the two lines are colinear.
-- If the results are `Infinity` then the two lines are parallel and do not intersect.
lineIntersection :: Scalar a => Line a -> Line a -> (Float,Float)
lineIntersection
  Line { lineLoc = a, lineDir = ab }
  Line { lineLoc = c, lineDir = cd } = (t1,t2)
  where
  det = withVec \x1 y1 -> withVec \x2 y2 -> x1 * y2 - y1 * x2

  -- a + l1 * ab = c + l2 * cd
  -- l1 = l2 * cd + ac

  ac  = c - a
  dir = toFloat (det ab cd)

  t1  = toFloat (det ac cd) / dir
  t2  = toFloat (det ac ab) / dir

{-# specialize lineIntersection :: Line Float -> Line Float -> (Float,Float) #-}
{-# specialize lineIntersection :: Line Int -> Line Int -> (Float,Float) #-}

-- | Create a line segment between two points
lineFromTo :: Scalar a => Vec a -> Vec a -> Line a
lineFromTo start end = Line { lineLoc = start, lineDir = end - start }
{-# inline lineFromTo #-}


-- | Create a polygon out of a list of vertices.
listPolygon :: Scalar a => [Vec a] -> Polygon a
listPolygon xs = Polygon (V.fromList [ p | v <- xs, p <- withVec' v \x y -> [x,y] ])
{-# inline listPolygon #-}

-- | Create a polygon with the given number of vertices,
-- using a function to compute the vertices.
funPolygon :: Scalar a => Int -> (Int -> Vec a) -> Polygon a
funPolygon n v = Polygon (V.create
  do
    vs <- MV.new n
    let set i =
          let j = 2 * i
          in withVec' (v i) \x y -> MV.write vs j x >> MV.write vs (j+1) y 
    mapM_ set (take n [0..])
    pure vs
  )
{-# inline funPolygon #-}

-- | How many vertices are in this polygon.
polyVertexNum :: Scalar a => Polygon a -> Int
polyVertexNum (Polygon vs) = V.length vs `div` 2
{-# inline polyVertexNum #-}

-- | Get a vertex of the polygon.
-- `polyVertex i p` is valid if `0 <= i < polyVertexNum p`
polyVertex :: Scalar a => Int -> Polygon a -> Vec a
polyVertex i' (Polygon vs) = vec x y
  where
  i = 2 * i'
  x = vs V.! i
  y = vs V.! (i + 1)
{-# inline polyVertex #-}

-- | Get an edge of the polygon.
-- `polyEdge i p` is valid if `0 <= i < polyVertexNum p`
polyEdge :: Scalar a => Int -> Polygon a -> Line a
polyEdge i p = lineFromTo (polyVertex i p) (polyVertex j p)
  where
  j' = i + 1
  j  = if j' == polyVertexNum p then 0 else j' 
{-# inline polyEdge #-}

polyEdges :: Scalar a => Polygon a -> [Line a]
polyEdges p = map (`polyEdge` p) (take (polyVertexNum p) [ 0 .. ])

polyIntersect :: Scalar a => Polygon a -> Polygon a -> ()
polyIntersect = undefined
  where
  checkPoly p1 p2 =
    undefined
  checkEdge e p2 =
    let n = normal e
    in undefined