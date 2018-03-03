{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
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
import Data.List hiding (group)
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

instance Manager StaticGeom

instance Shaped StaticGeom where
  shape = gshape.shape

staticGeom = typeRep (Proxy :: Proxy StaticGeom)

initialSet :: (Monad m) => Wire s e m (SignalL '[Create StaticGeom,Destroy]) (SignalL '[Create StaticGeom,Destroy])
initialSet = dSwitch $ proc (s `SCons` a) -> do
  evt <- now -< map ( flip StaticGeom)
    [ newShape $ newOBB (V2 0 -100) (V2 640 50),
      newShape $ newOBB (V2 (-320) 0) (V2 10 320),
      newShape $ newOBB (V2 (320) 0) (V2 10 320)]
  returnA -< (((map (fmap CNoId) $ eventToSignal_ evt) ++ s) `SCons` a, arr id <$ evt)

instance EntityW '[] '[Create StaticGeom,Destroy] StaticGeom [DrawableObject,Create Body] where
  wire =
    let create a = proc _ -> do
          c <- now -< simpleBody (1/0) (a^.shape) (a^?!ident) & shapes %~ map (group .~ 1)
          returnA -< (a, sigify [tdo  (a^.gshape)] .*. sigify [Create c] .*. SNil)
    in second initialSet >>> mkObject create (const mempty)

data DynamicGeom = DynGeo {_dgid::(TypeRep,Int),_dgPhys::Body,_dgcolor::V4 Float} deriving (Typeable,Show)
makeLenses ''DynamicGeom

instance HasId DynamicGeom where
  ident = dgid
instance Manager DynamicGeom

instance Tinted DynamicGeom where
  color = dgcolor

instance Visible DynamicGeom where
  toDrawableObjects (DynGeo _ phy c)= map (\s -> tdo (newShape (s^.shape) & color .~ c)) (phy^.shapes.toWorld phy)

dynamicGeom = typeRep (Proxy :: Proxy DynamicGeom)
instance EntityW '[M.Map EntityId Body] '[Create DynamicGeom, Destroy] DynamicGeom [DrawableObject,Create Body] where
  wire =
    let create a = mkSFN $ \([phyObj] `HCons` HNil,_) ->
          let newObj = a & dgPhys .~ phyObj
          in ((newObj,  SNil),create newObj)
    in mkObject create (const mempty) >>> signal_ (Update . _dgPhys) >>> drawW
