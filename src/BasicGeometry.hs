{-#LANGUAGE Arrows#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE NegativeLiterals#-}
module BasicGeometry where

import Prelude hiding ((.),id)
import Entity
import Geometry
import Physics hiding (_shape)
import Shape
import Signals
import Data.Typeable
import Graphics
import Renderer
import Control.Lens
import Control.Applicative
import Data.Ord
import Data.List
import Control.Arrow
import qualified Data.Map as M
import Linear.V2
import Linear.V4
import Debug.Trace
import Data.Maybe
import EntityUtils
import Wires
import Graphics

data StaticGeom = StaticGeom{_gid::(TypeRep,Int),_gshape::Shape} deriving (Show,Typeable)
makeLenses ''StaticGeom

instance HasId StaticGeom where
  ident = gid

instance Shaped StaticGeom where
  shape = gshape.shape

staticGeom = typeRep (Proxy :: Proxy StaticGeom)
initialSet =
  zipWith StaticGeom
    (zip (cycle [staticGeom]) [1..])
    [ newShape $ newOBB (V2 0 -160) (V2 640 50),
      newShape $ newOBB (V2 (-320) 0) (V2 10 320),
      newShape $ newOBB (V2 (320) 0) (V2 10 320)]

instance EntityW '[Delayed StaticGeom] '[Create StaticGeom,Destroy] StaticGeom [DrawableObject,PhysObj] where
  wire = arr $ \(geom `ECons` ENil, create `SCons` destroy `SCons` SNil)->
    let geoms = nubBy (\x y -> _gid x == _gid y ) $ map fromDelay geom ++ unsignal create ++ initialSet
        shapes = map (DO .(_gshape)) geoms
        physObjs = map (\geom-> ((newPhysObj (geom^?!ident) (geom^.shape)) & inertia .~ (1/0)) & aInertia .~ (1/0)) geoms
        sigify = map (Signal Nothing)
    in (geoms, sigify shapes `SCons` sigify physObjs `SCons` SNil)

data DynamicGeom = DynGeo {_dgid::(TypeRep,Int),_dgPhys::PhysObj,_dgcolor::V4 Float} deriving (Typeable,Show)
makeLenses ''DynamicGeom

instance HasId DynamicGeom where
  ident = dgid

instance Shaped DynamicGeom where
  shape = dgPhys.shape

instance Tinted DynamicGeom where
  color = dgcolor

instance Visible DynamicGeom where
  toDrawable (DynGeo _ phy c)= DO (newShape (phy^.shape) & color .~ c)

dynamicGeom = typeRep (Proxy :: Proxy DynamicGeom)
instance EntityW '[M.Map EntityId PhysObj] '[Create DynamicGeom, Destroy] DynamicGeom [DrawableObject,PhysObj] where
  wire =
    let create a = mkSFN $ \(phyObj `HCons` HNil,_) ->
          let newObj = a & dgPhys .~ phyObj
          in ((newObj,  SNil),create newObj)
    in mkObject create (const mempty) >>> signal_ _dgPhys >>> drawW
