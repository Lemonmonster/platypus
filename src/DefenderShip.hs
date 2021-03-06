{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module DefenderShip (
  Ship,
  Projectile,
  newShip,
  ship,
  projectile
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
import Linear hiding (translation,angle)
import Data.Bool
import Signals
import Geometry
import qualified Data.Map as M
import SDL.Input
import EntityUtils
import Debug.Trace
import BasicGeometry
import Data.Fixed (mod')
import qualified Linear as L (angle)
import System.Random
import Data.Function (fix)

data Projectile = Projectile {_dynGeo::DynamicGeom} deriving (Typeable,Show)
makeLenses ''Projectile
projectile = typeRep (Proxy :: Proxy Projectile)

newProjectile x v i =
  let shp = newCircle x 10
  in  Projectile (DynGeo i (simpleBody 10 shp i & Physics.vel .~ v) (V4 1 0.7 0.4 1))

instance Visible Projectile where
  toDrawableObjects (Projectile d ) = toDrawableObjects d

instance HasId Projectile where
  ident = dynGeo.ident
instance Manager Projectile

instance EntityW [DynamicGeom, M.Map (Maybe EntityId) (Event [Collision])]
                 [Create Projectile, Destroy]
                 Projectile
                 '[Create Body,DrawableObject,(Create DynamicGeom),Destroy] where
    wire = mkObject (\proj -> proc ([dyn] `HCons` [c] `HCons` HNil,HNil) -> do
                acc <- switch ( 0 &&& arr ( (pure $ V2 0 (-1000)) <$  ))  -< c
                dest <- Wires.at 10 -< (proj^?!ident)
                returnA -< (Projectile $ dyn & dgPhys.accel .~ acc,sigify [Dest dest] `SCons` SNil))
                    (const mempty)
                   >>> signal_ (Update  . _dynGeo) >>> drawW >>> signal_ (Update . _dgPhys._dynGeo)

data Ship = Ship{_sSprite::Sprite,_obj::Body} deriving (Show,Typeable)
makeLenses ''Ship
ship = typeRep (Proxy :: Proxy Ship)

instance Visible Ship where
  toDrawableObjects s = [tdo (s ^. sSprite)]

instance Shaped Ship where
  shape = sSprite.shape

instance HasId Ship where
  ident = obj.ident
instance Manager Ship

instance Bodied Ship where
  body = obj

instance Poseable Ship where
  pos = body.pos
  angle = body.angle

newShip :: V2 Double -> EntityId -> Ship
newShip x i =
  let shape = newCircle x 50
  in Ship (newSprite "Defender" "ship" (shape & pos .~ 0 & orient .~ 0)) (simpleBody 50 shape i)


velocity = 500 :: Double
acceleration = 1000 :: Double

instance EntityW '[Keyboard,M.Map EntityId Body] '[Create Ship,Destroy] Ship '[Trackable,Create Body,CameraEvent,DrawableObject,Create Projectile] where
  wire =
    let key ::(Monad m) => V2 Double -> Scancode -> Wire s e m (Scancode -> Bool) (V2 Double)
        key vel code = arr (bool 0 vel . ($ code))
        start = proc (a,sig `SCons` r) -> do
          e <- now -< (newShip 0)
          returnA -< (a,(Signal Nothing (CNoId e):sig) `SCons` r)
    in start >>> mkObject ( \ship -> proc ([Keyboard pressed] `HCons` [obj] `HCons` HNil,HNil) -> do
        cameraEvt <- arr Focus . now -< ship^?!ident
        acc <- arr (\v-> if v ==0 then 0 else signorm v ^* DefenderShip.velocity) <<<
          key (V2 1 0) ScancodeD + key (V2 -1 0) ScancodeA +
          key (V2 0 -1) ScancodeS + key (V2 0 1) ScancodeW -< pressed
        vel <- maxVelocityV acceleration -< acc
        let firePressed = pressed ScancodeSpace
        firing <- edge id -< firePressed
        proj <- Wires.force <<< modes False (bool (never) id) <<< Wires.force <<< first (periodic 0.1) -< ((),firing)
        reset <- arr ((frame .~ 0) <$) <<< became not -< firePressed
        facingE <- arr (fmap signum) <<<  became (<0) &> became (>0) -< acc^._x
        start <- now -< 1
        facing <- hold -< mergeL start facingE
        scal <- valWire (ship^.shape.trans.scale) <<< arr (fmap ((\x ->_x %~ ((*x).abs)). signum )) -< facingE
        sp  <- spriteWire (ship^.sSprite) -< (ship^.shape, firing,reset)
        let da = (obj^.angle) `mod'` (2*pi)
            tq = (if da< pi then da else (da - 2*pi)) * (-500000)
            bdy = obj & Physics.accel .~  acc & torque .~ (tq)
            sprt = (sp & shape.trans.scale .~ scal)
            pVec = L.angle (bdy^.angle + if facing == 1 then 0 else pi)
        aVel' <- sigIntegral -< (bdy^.aVel,-(bdy^.aVel * 3)) -- damping
        flareEvt <-  (proc (i,pos,vel) -> do
                              let addAng = if i>0 then 0 else pi
                                  life = 1
                              ang <- randomRW -< (addAng - pi/16,addAng + pi/16)
                              let start = pos - (L.angle ang * 30)
                              periodic 0.01 -< (proc _ -> do
                                   pos' <- integral start . dragW (vel - (L.angle ang * 100)) -< 0.4
                                   animate <- now -< True
                                   change <- never -< ()
                                   for life . spriteWire ((newSprite "Defender" "flare" (newOBB start 20))
                                                          {_frameRate= fromRational.toRational $ 10/life,
                                                           _animating = True,
                                                           _ssz = -1 }) -<
                                     (newOBB pos' 20, animate,change)
                               )
                          ) -< (facing,bdy^.pos,obj ^. Physics.vel)
        flames <- (fix ( \ f l ->
                       dipSwitch
                          (\x -> return . map ((),))
                          l
                          (arr fst)
                          (\ lst x -> f (x:lst))
                      )) [] -< (flareEvt)
        returnA -< (Ship sprt (bdy & aVel.~aVel'),
                    sigify [cameraEvt] `SCons`
                    sigify (tdo flames : toDrawables bdy (hEnd $ hBuild sprt :: HList '[Sprite])) `SCons`
                    sigify [CNoId $ (newProjectile (pVec * 35 + obj^.pos ) (pVec * 1000) ) <$ proj] `SCons` SNil))
              (const mempty)
              >>>  signal_ (Update . (^.obj)) >>>
              signal_ ((\s -> Trackable (s^?!ident) (OBB $ Transform (s^.obj.pos) (s^.shape.trans.scale * 8) 0)))
