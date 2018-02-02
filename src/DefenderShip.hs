{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE NegativeLiterals#-}
module DefenderShip (
  Ship,
  newShip
)

where

import Prelude hiding ((.), id)
import Entity
import Physics
import Sprite
import Graphics
import Wires
import Input
import Control.Lens
import Data.Typeable
import Renderer hiding (obj)
import Linear hiding (translation)
import Data.Bool
import Signals
import Geometry
import qualified Data.Map as M
import SDL.Input
import EntityUtils
import Debug.Trace
import BasicGeometry

data Projectile = Projectile {_dynGeo::DynamicGeom} deriving (Typeable)
makeLenses ''Projectile

newProjectile x v i =
  let shp = newCircle x 10
  in  Projectile (DynGeo i ((newPhysObj i shp){_inertia=10,_aInertia=angularInertiaOf shp 10} & mover.velocity .~ v) (V4 1 0.7 0.4 1))

instance Visible Projectile where
  toDrawable (Projectile d ) = toDrawable d

instance HasId Projectile where
  ident = dynGeo.ident

instance EntityW [DynamicGeom, M.Map (Maybe EntityId) (Event Collision)]
                 [Create Projectile, Destroy]
                 Projectile
                 '[PhysObj,DrawableObject,(Create DynamicGeom),Destroy] where
    wire = mkObject (\proj -> proc (dyn `HCons` c `HCons` HNil,HNil) -> do
                acc <- switch ( 0 &&& arr ( (pure $ V2 0 (-1000)) <$  )) -< c
                dest <- Wires.at 10 -< (proj^?!ident)
                returnA -< (Projectile $ dyn & dgPhys.mover.acceleration .~ acc,sigify [Dest dest] `SCons` SNil))
                    (const mempty)
                   >>> signal_ (Update . _dynGeo) >>> drawW >>> signal_ (_dgPhys._dynGeo)

data Ship = Ship{_sSprite::Sprite,_obj::PhysObj} deriving (Show,Typeable)
makeLenses ''Ship

instance Visible Ship where
  toDrawable = DO . (^. sSprite)

instance Shaped Ship where
  shape = obj.shape

instance HasId Ship where
  ident = obj.ident

newShip :: V2 Float -> EntityId -> Ship
newShip x i =
  let shape = newCircle x 50
  in Ship (newSprite "Defender" "ship" shape) ((newPhysObj i shape){_inertia = 100,_aInertia=1/0})


velocity = 500
accel = 1000

instance EntityW '[Keyboard,M.Map EntityId PhysObj] '[Create Ship,Destroy] Ship '[DrawableObject,PhysObj,Create Projectile] where
  wire =
    let key ::(Monad m) => V2 Float -> Scancode -> Wire s e m (Scancode -> Bool) (V2 Float)
        key vel code = arr (bool 0 vel . ($ code))
        start = proc (a,sig `SCons` r) -> do
          e <- now -< (newShip 0)
          returnA -< (a,(Signal Nothing (CNoId e):sig) `SCons` r)
    in start >>> mkObject ( \ship -> proc ((Keyboard pressed) `HCons` obj `HCons` HNil,HNil) -> do
        acc <- arr (\v-> if v ==0 then 0 else signorm v ^* DefenderShip.velocity) <<<
          key (V2 1 0) ScancodeD + key (V2 -1 0) ScancodeA +
          key (V2 0 -1) ScancodeS + key (V2 0 1) ScancodeW -< pressed
        vel <- maxVelocityV (accel :: Float) -< acc
        let firePressed = pressed ScancodeSpace
        firing <- edge id -< firePressed
        proj <- modes False (bool (never) id) <<< first (periodic 0.1) -< ((),firing)
        reset <- arr ((frame .~ 0) <$) <<< became not -< firePressed
        facingE <- arr (fmap signum) <<<  became (<0) &> became (>0) -< acc^._x
        start <- now -< 1
        facing <- hold -< mergeL start facingE
        scal <- valWire (ship^.shape.trans.scale) <<< arr (fmap ((\x ->_x %~ ((*x).abs)). signum )) -< facingE
        sp  <- spriteWire (ship^.sSprite) -< (obj^.shape, firing,reset)
        returnA -< (Ship (sp & shape.trans.scale .~ scal) (obj & mover.Physics.velocity .~  vel),
                    sigify [CNoId $ (newProjectile (V2 (50*facing) 0 + obj^.shape.trans.translation) (V2 (facing*500) 0) ) <$ proj] `SCons` SNil))
              (const mempty)
              >>>  signal_ (^.obj) >>> drawW
