{-# LANGUAGE TemplateHaskell #-}
module Barrell where

import Entity
import Signals
import EntityUtils
import Linear hiding (angle)
import Physics
import Control.Lens hiding (at)
import DefenderShip
import Data.Typeable
import Geometry as G
import Graphics
import Wires
import Prelude hiding ((.),id)
import qualified Data.Map as M
import Renderer
import Data.Function hiding ((.),id)
import Sprite
import Control.Monad.Fix
import Debug.Trace

data Barrell = Barrell{
  _bPos :: V2 Double,
  _bAngle :: Double,
  _bId :: EntityId
} deriving (Typeable,Show)
makeLenses ''Barrell 

instance HasId Barrell where
  ident = bId

instance Manager Barrell

instance Poseable Barrell where
  pos = bPos
  angle = bAngle

barell :: TypeRep 
barell = typeRep (Proxy :: Proxy Barrell)

initialLife = 3 :: Double
activeRange = 100 :: Double
deactivateRange = 600 :: Double
speed = 100 :: V2 Double

instance EntityW '[M.Map (Maybe EntityId ) (Event [ Collision ]),M.Map EntityId Body,Ship]
                 '[Create Barrell, Destroy]
                  Barrell
                 '[DrawableObject,Create Body] where
    wire =
      let activeSprite = ( sprite 0 32 "Defender" "barrellEnemy" ){_animating=True,_frameRate=30}
          inactiveSprite = sprite 0 32 "Defender" "barrellInactive"
          active :: (MonadFix m , Monoid s, Monoid e, HasTime t s, Fractional t) => Wire s e m (V2 Double, V2 Double, V2 Double) (V2 Double,Sprite)
          active = proc (pos,targetPos,velocity) -> do
            let toward = normalize $  targetPos - pos
                upDiag =  speed * (toward  ^* sqrt 2 + perp toward ^* sqrt 2 ) 
                downDiag =  speed * ( toward ^* sqrt 2 - perp toward ^* sqrt 2 )
            fix ( \ w -> for 0.5 . reDelayBy fst --> for 0.5 . reDelayBy snd --> w) &&& spriteW_ activeSprite -< ((upDiag,downDiag),velocity)
          inactive :: (MonadFix m,Monoid s, Monoid e, HasTime t s) => Wire s e m (V2 Double, V2 Double, V2 Double) (V2 Double,Sprite)
          inactive = proc (pos,targetPos,velocity) -> do
            sigIntegral &&& spriteW_ inactiveSprite -< (velocity,velocity * (-0.9) )
          within x (pos,targetPos,_) = quadrance (pos-targetPos) < x ** 2
      in arr ( exempt (Proxy :: Proxy '[Ship]) ) >>> mkObject
          (\ (Barrell bPos startAng id) -> proc ([ evt ] `HCons` bBody `HCons` ship `HCons` _ , _) -> do
                  life <- valWire 3 .
                          arr ((subtract 1) <$) .
                          dropWhileE (not . any (involves $ EntityId (projectile,-1))) -< evt
                  kill <- arr (id <$) . became (<0) -< life
                  bBody' <- arr head . reDelay [simpleBody 100 (newOBB bPos (V2 22 30) & angle .~ startAng) id] -< bBody
                  let targetPos = head $ map (^.pos) ship <|> [1/0]
                  (vel',sprite) <- fix (\w ->
                       (inactive . Wires.unless (within activeRange) >--
                       active . Wires.unless (not . within deactivateRange)) -->
                        w) -< (bBody'^.pos,targetPos,bBody'^.vel)
                  bBody'' <- stabilizeAngle 0 500000 3 -< bBody' & vel .~ vel'
                  returnA -< (Barrell (bBody''^.pos) (bBody''^.angle) id,
                              sigify (toDrawables bBody'' (sprite .*. HNil)) `SCons`
                              sigify [Update bBody''] `SCons`
                              SNil)
          )
          (const mempty)
