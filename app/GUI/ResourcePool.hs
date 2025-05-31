module GUI.ResourcePool(
  Resources(..),
  noResources,
  Resource, getResource,
  resetResources, destroyResources
) where

import Data.Bits(shiftR)
import SFML.SFResource
import SFML.Graphics.Types
import SFML.Utils
import SFML.Graphics.Text
import SFML.Graphics.RectangleShape
import SFML.Graphics.CircleShape
import SFML.Graphics.ConvexShape
import SFML.Graphics.Sprite



data Resources = Resources {
  rsrText   :: Pool Text,
  rsrRect   :: Pool RectangleShape,
  rsrCirc   :: Pool CircleShape,
  rsrConvex :: Pool ConvexShape,
  rsrSprite :: Pool Sprite
}

noResources :: Resources
noResources = Resources {
  rsrText = emptyPool,
  rsrRect = emptyPool,
  rsrCirc = emptyPool,
  rsrConvex = emptyPool,
  rsrSprite = emptyPool
}

resetResources :: Resources -> IO Resources
resetResources rs =
  do
    txt  <- resetPool (rsrText rs)
    rect <- resetPool (rsrRect rs)
    circ <- resetPool (rsrCirc rs)
    conv <- resetPool (rsrConvex rs)
    spr  <- resetPool (rsrSprite rs)
    pure
      Resources {
        rsrText = txt,
        rsrRect = rect,
        rsrCirc = circ,
        rsrConvex = conv,
        rsrSprite = spr
      }

destroyResources :: Resources -> IO ()
destroyResources rs =
  do
    destroyPool (rsrText rs)
    destroyPool (rsrRect rs)
    destroyPool (rsrCirc rs)
    destroyPool (rsrConvex rs)
    destroyPool (rsrSprite rs) 


data Pool a = Pool {
  total  :: !Int,
  used   :: !Int,
  availR :: [a],
  usedR  :: [a]
}

emptyPool :: Pool a
emptyPool = Pool {
  total  = 0,
  used   = 0,
  availR = [],
  usedR  = []
}

getResourceFrom :: Resource a => Pool a -> IO (a, Pool a)
getResourceFrom pool =
  case availR pool of
    r : rs ->
      pure
        ( r,
          Pool {
            total   = total pool,
            used    = 1 + used pool,
            availR  = rs,
            usedR   = r : usedR pool
          }
        )
    [] ->
      do
        r <- create
        pure
          ( r,
            Pool {
              total  = 1 + total pool,
              used   = 1 + used pool,
              availR = availR pool,
              usedR  = r : usedR pool
            }
          )

resetPool :: SFResource a => Pool a -> IO (Pool a)
resetPool pool
  | t == 0 = pure pool
  | avail > half =
    case splitAt half (availR pool) of
      (as,bs) ->
        do
          mapM_ destroy as
          pure
            Pool {
              total   = t - half,
              used    = 0,
              availR  = bs ++ usedR pool,
              usedR   = []
            }
  | otherwise =
    pure
      Pool {
        total   = t,
        used    = 0,
        availR  = availR pool ++ usedR pool,
        usedR   = []
      }
  where  
  t     = total pool
  half  = t `shiftR` 1
  avail = t - used pool

destroyPool :: SFResource a => Pool a -> IO ()
destroyPool pool =
  do
    mapM_ destroy (availR pool)
    mapM_ destroy (usedR pool) 

-------------------------------------------------------------------------------
class Resource a where
  create :: IO a
  getResource :: Resources -> IO (a, Resources)

instance Resource Text where
  create = err createText
  getResource r =
    do
      (a,p) <- getResourceFrom (rsrText r)
      pure (a, r { rsrText = p })

instance Resource RectangleShape where
  create = err createRectangleShape
  getResource r =
    do
      (a,p) <- getResourceFrom (rsrRect r)
      pure (a, r { rsrRect = p })

instance Resource CircleShape where
  create = err createCircleShape
  getResource r =
    do
      (a,p) <- getResourceFrom (rsrCirc r)
      pure (a, r { rsrCirc = p })

instance Resource ConvexShape where
  create = err createConvexShape
  getResource r =
    do
      (a,p) <- getResourceFrom (rsrConvex r)
      pure (a, r { rsrConvex = p })

instance Resource Sprite where
  create = err createSprite
  getResource r =
    do
      (a,p) <- getResourceFrom (rsrSprite r)
      pure (a, r { rsrSprite = p })


