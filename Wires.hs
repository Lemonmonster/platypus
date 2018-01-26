{-#LANGUAGE ExplicitForAll#-}
{-#LANGUAGE ScopedTypeVariables#-}

module Wires (
integral,
integralWith,
module ALL,
maxVelocity,
maxVelocityV,
linearFilter,
lowestOver,
doOnce,
signal,
signal_
)
where
import Control.Wire as ALL hiding (integral,integralWith)
import Prelude as ALL hiding ((.),id,until)
import Linear.Vector
import Linear.Metric
import Entity
import Data.Maybe
import Data.Typeable

integral :: (Fractional a, HasTime t s) => a-> Wire s e m a a
integral x = mkPure $ \ds dx ->
  let dt = realToFrac $ dtime ds
      x' = (x + (dt*dx))
  in  x' `seq` (Right x', integral x')

integralWith :: (Fractional a, HasTime t s) => (w->a->a)-> a-> Wire s e m (a,w) a
integralWith f x = mkPure $ \ds (dx,w) ->
  let dt = realToFrac $ dtime ds
      x' = f w (x + (dt*dx))
  in  x' `seq` (Right x', integralWith f x')

maxVelocity:: (Ord a,Fractional a,HasTime t s) => a -> Wire s e m a a
maxVelocity v = mkPure $ \_ x ->
  let wire x = mkPure $ \ds x' ->
        let dsf = realToFrac $ dtime ds
            dx = x - x'
            x'' = x + (signum (dx - x) * min (abs (dsf*v)) (abs dx) )
        in (Right x'', wire x'')
  in (Right x,wire x)

maxVelocityV :: (Metric f,Additive f,Num a,Ord a,Floating a,HasTime t s) => a -> Wire s e m (f a) (f a)
maxVelocityV v = mkPure $ \_ x ->
  let wire x = mkPure $ \ds nx ->
        let dsf = realToFrac $ dtime ds
            dx = nx ^-^ x
        in if quadrance dx > ((dsf*v)^2) then
             let x' = x ^+^ ((dsf* abs v) *^ signorm dx)
             in (Right x', wire x')
           else
             (Right nx, wire nx)
  in (Right x,wire x)

linearFilter  :: (Fractional a) => a -> a -> Wire s e m a a
linearFilter weight start =  mkSFN $ \x ->
  let x' = start + weight * (x - start)
  in  (x', linearFilter weight x')

lowest :: (Ord a) => Wire s e m a a
lowest =
  let lowest' a = mkSFN $ \x ->
        let m = min a x
        in  (m,lowest' m)
  in mkSFN $ \x -> (x,lowest' x)

lowestOver :: (Ord a,HasTime t s,Monoid e,Monad m) => t  -> Wire s e m a a
lowestOver t  =
  for t . lowest --> lowestOver t

doOnce :: (Monoid e,Monoid s)=>Wire s e m a a
doOnce = mkPure (\ _ a -> (Right a, inhibit mempty))

signal ::forall a m b s e sl. (Monad m,HasId a,Typeable a)=> Maybe EntityId -> (a -> b) -> Wire s e m ([a],SignalL sl) ([a],SignalL (b ': sl))
signal target getter =arr $ \(vals,list) ->
  (vals,map (Signal target . getter) (filter (((typeRep (Proxy :: Proxy a))==).fst.fromJust._ident) vals) `SCons` list)

signal_ :: (Monad m,HasId a,Typeable a)=>  (a -> b) -> Wire s e m ([a],SignalL sl) ([a],SignalL (b ': sl))
signal_ = signal Nothing
