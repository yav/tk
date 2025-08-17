{- | 2D Points and Shapes

This module assumes the following geometry:
  * the x-axis is positive to the right
  * the y-axis is positive downward

Angles are positive when going from the x-axis toward the y-axis.

* "clockwise" rotation is from the x-axis toward the y-axis.
* "anti-clockwise" rotation is from the y-axis to the x-axis.
-}
module GUI.Geometry (
  -- * Vectors
  Vec,
  Scalar(..),
  withVec',
  updVec, mapVec, zipVec,
  isUnitVec, len,
  dot, cross, normal,
  (.*), (.+),

  -- * Shapes
  
  -- ** Lines
  Line,
  withLine, withLine',
  lineStart, lineEnd, lineDir,
  lineFromTo,
  lineIntersection,

  -- ** Rectangles
  Rect(..),

  -- * Circles
  Circle(..),
  circleContains,
  circleIntersect,

  -- ** Polygons
  Polygon,
  polyFromList,
  polyFromFun,
  polyVertexNum,
  polyVertex,
  polyEdge,
  polyVertices,
  polyEdges,
  polyContains,
  polyIntersect

) where

import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as MV


-- | 2D Vectors
data family Vec scalar
data instance Vec Int     = VecInt !Int !Int
data instance Vec Float   = VecFloat !Float !Float


-- | A line segment
data Line a = Line {
  lineFrom :: !(Vec a),  -- ^ Starting point of line segment
  lineTo   :: !(Vec a)   -- ^ End point of the line
} deriving (Show,Read,Eq,Ord)

-- | A rectangle
data Rect a               = Rect { rectLoc :: !(Vec a), rectDim :: !(Vec a) }
  deriving (Show,Read,Eq,Ord)

-- A circle
data Circle a             = Circle { circleCenter :: !(Vec a), circleRadius :: a }
  deriving (Show,Read,Eq,Ord)

-- | A polygon
newtype Polygon a         = Polygon (V.Vector a)
  deriving (Eq,Ord)

instance (Scalar a, Show a) => Show (Polygon a) where
  showsPrec p = showsPrec p . polyVertices

instance (Scalar a, Read a) => Read (Polygon a) where
  readsPrec p txt = [ (polyFromList xs, ys) | (xs,ys) <- readsPrec p txt ]

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


-- | Multiple by a scalar
(.*) :: Scalar a => a -> Vec a -> Vec a
x .* v = vec x x * v
{-# inline (.*) #-}

-- | Translate by a scalar
(.+) :: Scalar a => a -> Vec a -> Vec a
x .+ v = vec x x + v
{-# inline (.+) #-}

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

{- | Dot product.  `dot x y == len x * len y * cos a`, where
`a` is the angle from the first ovector to the second.

May be used to project `x` onto `y`. -}
dot :: Scalar a => Vec a -> Vec a -> a
dot x y = withVec (+) (x * y)
{-# inline dot #-}

{- | The cross product. `cross x y = len x * len y * sin a`, where
`a` is the angle from the first vector to the second.

Area of the parallelogram formed by the two vectors.
The result is positive when `y` is "clockwise" relative to `x`. -}
cross :: Scalar a => Vec a -> Vec a -> a
cross = withVec \x1 y1 -> withVec \x2 y2 -> x1 * y2 - y1 * x2
{-# inline cross #-}

-- | Rotate a vector 90 degrees "clockwise" (i.e., x-axis toward y-axis)
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
-- The results point to the intersection as an interpolant along the first line:
-- 0 is at the start of the segment, 1 is at the end.
-- Returns `Nothing` if the line segments do not intersect.
-- If the line segments lie on the same line, then there maybe multiple
-- points of intesection.  In that case we return the largest interpolant.

lineIntersection :: Scalar a => Line a -> Line a -> Maybe Float
lineIntersection l1 l2
  | bot == 0          = if top == 0 then Just ct1 else Nothing
  | t1 < 0 || t1 > 1  = Nothing
  | otherwise         = Just t1
  where
  -- a + l1 * ab = c + l2 * cd
  -- l1 = l2 * cd + ac
  a   = lineStart l1
  ab  = lineDir l1
  c   = lineStart l2
  cd  = lineDir l2
  ac  = c - a
  bot = toFloat (cross ab cd)
  top = toFloat (cross ac cd)
  t1  =  top / bot

  -- colinear
  cvt v     = toFloat (dot v ab) / ab2
  ab2       = toFloat (dot ab ab)
  l2_start  = cvt ac
  l2_end    = l2_start + cvt cd
  ct1       = max l2_start l2_end

{-# specialize lineIntersection :: Line Float -> Line Float -> Maybe Float #-}
{-# specialize lineIntersection :: Line Int -> Line Int -> Maybe Float #-}

-- | Do something with the start and end points of a line segment. -}
withLine' :: Scalar a => Line a -> (Vec a -> Vec a -> b) -> b
withLine' l k = k (lineFrom l) (lineTo l)
{-# inline withLine' #-}

-- | Do something with the start and end points of a line segment. -}
withLine :: Scalar a => (Vec a -> Vec a -> b) -> Line a -> b
withLine = flip withLine'
{-# inline withLine #-}

-- | Create a line segment between two points
lineFromTo :: Scalar a => Vec a -> Vec a -> Line a
lineFromTo start end = Line { lineFrom = start, lineTo = end }
{-# inline lineFromTo #-}

-- | The start of a line.
lineStart :: Scalar a => Line a -> Vec a
lineStart = withLine const
{-# inline lineStart #-}

-- | The end point of a line.
lineEnd :: Scalar a => Line a -> Vec a
lineEnd = withLine \_ y -> y
{-# inline lineEnd #-}

-- | A vector from the start point of a line to the end.
lineDir :: Scalar a => Line a -> Vec a
lineDir = withLine (flip (-))
{-# inline lineDir #-}


-- | Is the given point inside the given circle
circleContains :: Scalar a => Vec a -> Circle a -> Bool
circleContains pt Circle { circleRadius = r, circleCenter = c } =
  len2 (pt - c) < r * r

-- | Do these circles overlap?
circleIntersect :: Scalar a => Circle a -> Circle a -> Bool
circleIntersect circ1 circ2 = len2 v < d * d
  where
  v = circleCenter circ1 - circleCenter circ2
  d = circleRadius circ1 + circleRadius circ2



-- | Create a polygon out of a list of vertices.
-- The geometric operations (intersection, containment, etc.) assume that the
-- vertices are listed "clockwise" around the polygon.
polyFromList :: Scalar a => [Vec a] -> Polygon a
polyFromList xs = Polygon (V.fromList [ p | v <- xs, p <- withVec' v \x y -> [x,y] ])
{-# inline polyFromList #-}

-- | Create a polygon with the given number of vertices,
-- using a function to compute the vertices.
-- The geometric operations (intersection, containment, etc.) assume that the
-- vertices are listed "clockwise" around the polygon.
polyFromFun :: Scalar a => Int -> (Int -> Vec a) -> Polygon a
polyFromFun n v = Polygon (V.create
  do
    vs <- MV.new n
    let set i =
          let j = 2 * i
          in withVec' (v i) \x y -> MV.write vs j x >> MV.write vs (j+1) y 
    mapM_ set (take n [0..])
    pure vs
  )
{-# inline polyFromFun #-}

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

polyVertices :: Scalar a => Polygon a -> [Vec a]
polyVertices p = [ polyVertex i p | i <- [ 0 .. polyVertexNum p - 1 ] ] 
{-# inline polyVertices #-}

polyEdges :: Scalar a => Polygon a -> [Line a]
polyEdges p = map (`polyEdge` p) (take (polyVertexNum p) [ 0 .. ])
{-# inline polyEdges #-}

polyContains :: Scalar a => Vec a -> Polygon a -> Bool
polyContains pt = all inside . polyEdges
  where
  inside l = cross (lineDir l) (pt - lineStart l) > 0
{-# SPECIALISE polyContains :: Vec Int -> Polygon Int -> Bool #-}
{-# SPECIALISE polyContains :: Vec Float -> Polygon Float -> Bool #-}

polyIntersect :: Scalar a => Polygon a -> Polygon a -> Bool
polyIntersect p1 p2 = sepPoly p1 p2 || sepPoly p2 p1
  where
  sepPoly p q = any (sepAlong p q) (polyEdges p)
  sepAlong p q e =
    let proj = dot (normal (lineDir e))
        rng ps =
          case map proj ps of
            [] -> error "0 vertex polygon"
            pr : more -> foldr (\x (l,u) -> if x < l then (x,u) else if x > u then (l,x) else (l,u)) (pr,pr) more
        (l1,u1) = rng (polyVertices p)
        (l2,u2) = rng (polyVertices q)
    in u1 < l2 || u2 < l1