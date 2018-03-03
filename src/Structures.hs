{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Structures (
  Poseable,
  pos,
  angle,
  BodyShape (..),
  bshape,
  friction,
  group,
  layers,
  elasticity,
  mass,
  surfaceVel,
  Body(..),
  shapes ,
  bpos ,
  vel,
  Structures.force ,
  bangle ,
  aVel,
  torque ,
  bid,
  processCollision,
  ConstraintType (..),
  Constraint(..),
  newBodyShape,
  newBody,
  simpleBody,
  toWorld,
  canRotate,
  accel,
  toDrawables
)where

import Prelude hiding ((.),id)
import Wires
import Control.Lens
import Linear hiding (angle)
import qualified Linear as L (angle)
import Entity
import Geometry
import Renderer
import Data.Word
import Data.Typeable
import Entity
import Renderer (Shaped,shape)

class Poseable a where
  pos :: Lens' a (V2 Double)
  angle:: Lens' a Double

instance Poseable (V2 Double) where
  pos = iso id id
  angle = lens (\(V2 x y) -> atan2 y x) (\v o -> L.angle o ^* norm v)

data BodyShape = BodyShape{
  _bshape::PhysShape,
  _friction::Double,
  _group::Word32,
  _layers :: Word32,
  _elasticity :: Double,
  _mass :: Double,
  _surfaceVel :: V2 Double
} deriving (Show,Eq,Ord,Typeable)
makeLenses ''BodyShape

instance Poseable BodyShape where
  pos = bshape.pos
  angle = bshape.angle

instance Shaped BodyShape where
  shape = bshape

data Body = Body {
  _shapes :: ![BodyShape],
  _bpos :: !(V2 Double),
  _vel :: !(V2 Double),
  _force :: !(V2 Double),
  _bangle :: !Double,
  _aVel :: !Double,
  _torque :: !Double,
  _processCollision :: !Bool,
  _canRotate :: !Bool,
  _bid :: !EntityId
} deriving (Show,Eq,Ord,Typeable)
makeLenses ''Body

accel :: Traversal' Body (V2 Double)
accel f b@Body{_force=fr,_shapes=shps} =
  if null shps then
     pure b
  else
     let totalMass = sum $ map (^.mass) shps
     in  (\f -> b{_force = f ^* totalMass}) <$> f (fr ^* (1/totalMass))

instance HasId Body where
  ident = bid

instance Poseable Body where
  pos = bpos
  angle = bangle

instance Poseable PhysShape where
  pos = position
  angle = orient

type Point = V2 Double

data ConstraintType =
  Pin Point Point |
  Slide Point Point Double Double |
  Pivot1 Point |
  Pivot2 Point Point |
  Groove (Point,Point) Point|
  Gear Double Double |
  DampedSpring Point Point Double Double Double|
  DampedRotarySpring Double Double Double|
  Ratchet Double Double |
  RotaryLimit Double Double |
  SimpleMotor Double deriving (Eq,Ord,Show)

data Constraint = Constraint EntityId EntityId ConstraintType EntityId deriving (Eq,Ord,Show)

instance HasId Constraint where
  ident = lens (\(Constraint _ _ _ i) -> i) (\(Constraint a b c _) i ->  Constraint a b c i)

newBodyShape :: PhysShape -> Double -> BodyShape
newBodyShape s m = BodyShape s 0.5 0 0xFFFFFFFF 0.5 m 0

newBody :: V2 Double -> Double -> [BodyShape] -> EntityId -> Body
newBody p a s =
  Body s p 0 0 a 0 0 True True

simpleBody :: Double -> PhysShape  -> EntityId -> Body
simpleBody m s =
  Body [newBodyShape (s & position .~ 0 & orient .~ 0) m] (s^.position) 0 0 (s^.orient) 0 0 True True

toWorld :: Body -> Iso' [BodyShape] [BodyShape]
toWorld Body{_bangle=ba,_bpos=bp} =
  iso (map (\p->p & angle %~ (+ ba) & pos %~ (`fillMul` convertTransform (Transform bp 1 ba))))
      (map (\p->p & angle %~ subtract ba & pos %~ (`fillMul` convertTransform (Transform (-bp) 1 (-ba)))))

newtype TDO = TDO Body

instance (Poseable a, Drawable a) => ApplyAB TDO ([DrawableObject],a) [DrawableObject] where
  applyAB (TDO Body{_bpos=p,_bangle=ang}) (d,a) = tdo (a & pos .~ p & angle .~ ang ) : d

toDrawables :: (HFoldl r TDO l [DrawableObject] [DrawableObject]) => Body -> r l -> [DrawableObject]
toDrawables b = hFoldl (TDO b) ([] :: [DrawableObject])