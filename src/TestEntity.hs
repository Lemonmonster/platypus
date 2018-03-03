{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeFamilies #-}
module TestEntity where

import Entity

import Prelude hiding (id,(.))
import Data.Typeable
import Linear.V2
import Linear.Metric (signorm)
import Linear.Vector ((^*))
import Geometry
import Data.Bool
import SDL.Input.Keyboard
import Signals
import SDL.Input.Mouse
import Wires hiding (at)
import Data.Maybe
import Shape
import Input hiding (position)
import Renderer
import TextBlock
import Debug.Trace
import Control.Lens
import BasicGeometry
import Data.Bool
import Physics hiding (velocity,position)
import Data.Function (fix)
import qualified Data.Map as M
import Graphics
import Control.DeepSeq


newtype ControllableSquare = CSquare (V2 Float)

velocity = 400 :: Float
accel = 800 :: Float

{-instance EntityW '[Keyboard] '[] ControllableSquare '[DrawableObject ] where
  wire =
    let key vel code = modes False (bool (pure (-vel)) 0) <<< second (edge id) <<< arr ((\x->(x,x)).($ code))
    in proc ([Keyboard pressed] `ECons` _,SNil) -> do
        pos <-integral 0 <<< maxVelocityV accel <<< arr (\v-> if v ==0 then 0 else signorm v ^* velocity) <<<
          key (V2 1 0) ScancodeD + key (V2 -1 0) ScancodeA +
          key (V2 0 -1) ScancodeS + key (V2 0 1) ScancodeW -< pressed
        shap <- arr (DO . newShape . (`newOBB` 50)) -< pos
        returnA -< ([CSquare pos],sigify [shap] `SCons` SNil)
-}
data Clicker = Clicker

instance EntityW '[Mouse] '[] Clicker '[Create DynamicGeom] where
  wire =
    let createVals s p i =
          DynGeo i (simpleBody 100 (s p 50) i & Physics.accel .~ (V2 0 -1000)) 1
    in proc ([Input.Mouse f p] `ECons` ENil,_) -> do
        e1 <- alternate never (periodic 0.2) <<< (id &&& edge id) <<< arr ($ ButtonRight) -< f
        e2 <- alternate never (periodic 0.2) <<< (id &&& edge id) <<< arr ($ ButtonLeft) -< f
        let out1 = createVals newCircle p <$ e1
        let out2 = createVals newOBB p <$ e2
        returnA -< ([Clicker], rnf out2 `seq` rnf out1 `seq` sigify [CNoId out1,CNoId out2] `SCons` SNil)


data PhysTest = PhysTest deriving (Typeable)
physTest = typeRep (Proxy :: Proxy PhysTest)

--setPhysProps p = p{_aInertia=300000,_inertia=100} & mover.acceleration .~ (V2 0 -1000)

{-instance EntityW '[Mouse,Keyboard,M.Map EntityId PhysObj,M.Map (Maybe EntityId) (Event Collision)] '[] PhysTest [DrawableObject,Event [PhysObj]] where
  wire = proc ([Input.Mouse f p] `ECons` [Input.Keyboard k] `ECons` [phys] `ECons` [cols] `ECons` ENil ,SNil) -> do
          rec objs <- delay [] <<< dValWire [] -< foldl1 mergeL [physUpdate,createObj,reset]
              physUpdate <- fmap (fmap (const.fromJust)) (became isJust) -<
                  mconcat $ map (\i -> fmap pure (phys^.at (physTest,i))) [1..length objs]
              reset <- arr (const [] <$) <<< (became ($ ScancodeR)) -< k
              sendUpdate <- arr (uncurry (<$)) <<<
                second (modes False (bool never (periodic 0.05)) <<<  id &&& edge id <<< arr ($ ScancodeSpace)) -< (objs,k)
              size <- integral 50 <<< arr (*10) <<< 0 >-- hold <<< arr (uncurry $ merge (+))
                        <<< (fmap (fmap (bool 0 1)) (edge id <<< arr ($ ScancodeW)) &&&
                            fmap (fmap (bool 0 (-1))) (edge id <<< arr ($ ScancodeS)))
                        -< k
              orien <- integral 0 <<< arr (*10) <<< 0 >-- hold <<< arr (uncurry $ merge (+))
                        <<< (fmap (fmap (bool 0 1)) (edge id <<< arr ($ ScancodeA)) &&&
                            fmap (fmap (bool 0 (-1))) (edge id <<< arr ($ ScancodeD)))
                        -< k
              create <- fmap ((newOBB) <$) (became ($ ButtonLeft)) &>
                        fmap ((\p w ->newCircle p (w^._x)) <$) (became ($ ButtonRight)) -< f
              let createObj = fmap ((:).setPhysProps.(shape.orient .~ orien).newPhysObj (physTest,length objs + 1).($ (p,size)).uncurry) create
          let manifolds = mconcat $ map (fmap (\x->[x]).snd) $ filter (maybe False ((==physTest).fst).fst) $ M.toList cols
          manifoldDrawables <- pure [] >-- hold -< fmap (concatMap (\(Collision _ _ (Manifold p n t o)) ->
                    map DO [newShape (newCircle p 1) & color .~ blue & sz .~ 10, newShape (newLine p (p + t^*o)) & color .~ red & sz .~ 10]
                  ) ) manifolds
          let drawableShapes = map (DO . uncurry (color.~)) $ zip (map (* 0.5) $ cycle [red,green,blue]) (map (newShape . (^.shape)) objs)
          let sizeBox = DO  $ textBox (show size++" "++show orien) defaultFont (newOBB 0 100)
          returnA -< ([PhysTest],sigify (sizeBox:drawableShapes++manifoldDrawables) `SCons` sigify [sendUpdate] `SCons` SNil)-}
