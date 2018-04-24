{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
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
signal_,
valWire,
dValWire,
signalConcat,
signalConcat_,
eventToSignal,
eventToSignal_,
sigIntegral,
modes,
alternate,
pSwitch,
dpSwitch,
ipSwitch,
dipSwitch,
randomW,
randomRW,
dragW,
lift,
signalToEvent,
reDelay,
reDelayBy
)
where
import Control.Wire as ALL hiding (integral,integralWith,modes,alternate)
import Prelude as ALL hiding ((.),id,until)
import Linear.Vector
import Linear.Metric
import Entity
import Data.Maybe
import Data.Typeable
import Control.Wire.Unsafe.Event
import qualified Data.Map.Strict as M
import Control.Monad (liftM)
import Control.Monad.IO.Class
import Data.Semigroup ((<>)) 
import Data.Either
import System.Random
import Control.Monad.Trans.Class

instance Show (a -> b) where
  show = const "<function>"

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
  dSwitch  $ lowest &&& (arr ( lowestOver t <$) . ( for t . never  --> now))

doOnce :: (Monoid e,Monoid s)=>Wire s e m a a
doOnce = mkPure (\ _ a -> (Right a, inhibit mempty))

--is the passed value now and the input value after now
--does NOT delay the value
reDelay :: (Monoid e , Monoid s, Monad m) => a -> Wire s e m a a
reDelay a = mkPure (\ _ _ -> (Right a, id))

reDelayBy :: (Monoid e , Monoid s, Monad m) => (b->a) -> Wire s e m (b,a) a
reDelayBy f = mkPure (\ _ (b,_) -> (Right $ f b, arr snd))

signal ::forall a m b s e sl. (Monad m,HasId a,Typeable a)=> Maybe EntityId -> (a -> b) -> Wire s e m ([a],SignalL sl) ([a],SignalL (b ': sl))
signal target getter =arr $ \(vals,list) ->
  (vals,map (Signal target . getter) (filter ((typeRep (Proxy :: Proxy a)==).idType.fromJust._ident) vals) `SCons` list)

signal_ :: (Monad m,HasId a,Typeable a)=>  (a -> b) -> Wire s e m ([a],SignalL sl) ([a],SignalL (b ': sl))
signal_ = signal Nothing

signalConcat ::forall a m b s e sl. (Monad m,HasId a,Typeable a)=>
                Maybe EntityId -> (a -> [b]) -> Wire s e m ([a],SignalL sl) ([a],SignalL (b ': sl))
signalConcat target getter =arr $ \(vals,list) ->
  (vals,concatMap (map (Signal target) . getter) (filter ((typeRep (Proxy :: Proxy a) ==).idType.fromJust._ident) vals) `SCons` list)

signalConcat_ :: (Monad m,HasId a,Typeable a)=>  (a -> [b]) -> Wire s e m ([a],SignalL sl) ([a],SignalL (b ': sl))
signalConcat_ = signalConcat Nothing

valWire :: (Monad m,Monoid s) => a -> Wire s e m (Event (a->a)) a
valWire a =
  switch (pure a &&& arr (fmap ( (. notYet) . valWire . ($ a))))

dValWire :: (Monad m,Monoid s) => a -> Wire s e m (Event (a->a)) a
dValWire a =
  dSwitch (pure a &&& arr (fmap ( dValWire . ($ a))))


eventToSignal :: Maybe EntityId -> (a -> b) -> Event [a] -> [Signal (Event b)]
eventToSignal i _ NoEvent = [Signal i NoEvent]
eventToSignal i f (Event lst) = map (Signal i . Event . f) lst
eventToSignal_ :: Event [a] -> [Signal (Event a)]
eventToSignal_ = eventToSignal Nothing id

signalToEvent :: (a -> Maybe (Event b)) -> (b -> b -> b) ->[Signal a] -> Event b
signalToEvent convert merg lst =
   foldl (merge merg) NoEvent $ catMaybes $ map (convert._payload) lst
   

--feedback based integral
sigIntegral :: (Fractional a, HasTime t s) => Wire s e m (a,a) a
sigIntegral =
  let f :: (Fractional a, HasTime t s) => c -> Wire s e m (a,a) a
      f _ = mkPure $ \ds (x,dx) ->
            let dt = realToFrac $ dtime ds
                x' = (x + (dt*dx))
            in  x' `seq` (Right x',f x')
  in f 0

dragW :: (Monoid s,Fractional a,HasTime t s) => a -> Wire s e m a a
dragW start = mkSF $ \ dx s ->
  let x' = (start - (realToFrac (dtime dx)*start*s))
  in  (x',dragW start)

randomRW :: (Random a,MonadIO m) =>  Wire s e m (a,a) a
randomRW = mkGen_ $ \ range  ->
  liftIO $ Right <$> randomRIO range

randomW :: (MonadIO m,Random a) => Wire s e m b a
randomW = mkGen_ $ \ _ ->
  liftIO $ Right <$> randomIO


liftW :: (MonadTrans t,Monoid s, Monad m, Monad (t m)) => Wire s e m a b -> Wire s e (t m) a b
liftW w = mkGen $ \ ds a -> do
    (b,w') <- lift $ stepWire w ds (Right a)
    return (b,liftW w')

--redefined here to reduce laziness
modes ::
    (Monad m, Ord k)
    => k  -- ^ Initial mode.
    -> (k -> Wire s e m a b)  -- ^ Select wire for given mode.
    -> Wire s e m (a, Event k) b
modes m0 select = loop M.empty m0 (select m0)
    where
    loop ms' m' w'' =
        WGen $ \ds mxev' ->
            case mxev' of
              Left _ -> do
                  (mx, w) <- stepWire w'' ds (fmap fst mxev')
                  return (mx, loop ms' m' w)
              Right (x', ev) -> do
                  let (ms, m, w') = switch ms' m' w'' ev
                  (mx, w) <- stepWire w' ds (Right x')
                  return (mx, loop ms m w)

    switch ms' m' w' NoEvent = (ms', m', w')
    switch ms' m' w' (Event m) =
        let ms = M.insert m' w' ms' in
        case M.lookup m ms of
          Nothing -> (ms, m, select m)
          Just w  -> (M.delete m ms, m, w)

--redefined here to reduce laziness
alternate ::
  (Monad m)
  => Wire s e m a b
  -> Wire s e m a b
  -> Wire s e m (a, Event x) b
alternate w1 w2 = go w1 w2 w1
    where
    go w1' w2' w' =
        WGen $ \ds mx' ->
            let (w1, w2, w) | Right (_, Event _) <- mx' = (w2', w1', w2')
                            | otherwise  = (w1', w2', w')
            in liftM (second (go w1 w2)) (stepWire w ds (fmap fst mx'))

--a netwire implementation of the Yampa function
--inhibits when any of the wires inhibit
pSwitch :: (Monad m , Monoid s,Traversable col, Foldable col,Monoid e) =>
           (forall sf. a -> col sf -> m (col (b, sf))) ->
           col (Wire s e m b c) ->
           Wire s e m (a, col c) (Event evt) ->
           (col (Wire s e m b c) ->  evt -> Wire s e m a (col c)) ->
           Wire s e m a (col c)
pSwitch route col sig continue  =
  let pSwitch' col sig = mkGen $ \ds a -> do
        col' <- mapM (\(x,w) -> stepWire w ds (Right x)) =<< route a col
        let inhibit = any isLeft $ fmap fst col'
            output = if inhibit then Left mempty else Right $ fmap ((\(Right x) -> x).fst) col'
            wires = fmap snd col'
        (eitherEvt,sig') <- stepWire sig ds (fmap (a,) output)
        return ( (,) <$> output <*> (fmap (continue wires) <$> eitherEvt), pSwitch' wires sig' )
  in switch $ pSwitch' col sig

dpSwitch :: (Monad m , Monoid s,Traversable col, Foldable col,Monoid e) =>
           (forall sf. a -> col sf -> m (col (b, sf))) ->
           col (Wire s e m b c) ->
           Wire s e m (a, col c) (Event evt) ->
           (col (Wire s e m b c) ->  evt -> Wire s e m a (col c)) ->
           Wire s e m a (col c)
dpSwitch route col sig continue  =
  let pSwitch' col sig = mkGen $ \ds a -> do
        col' <- mapM (\(x,w) -> stepWire w ds (Right x)) =<< route a col
        let inhibit = any isLeft $ fmap fst col'
            output = if inhibit then Left mempty else Right $ fmap ((\(Right x) -> x).fst) col'
            wires = fmap snd col'
        (eitherEvt,sig') <- stepWire sig ds (fmap (a,) output)
        return ( (,) <$> output <*> (fmap (continue wires) <$> eitherEvt), pSwitch' wires sig' )
  in dSwitch $ pSwitch' col sig

--removes wires upon inhibition but is otherwise identical to pSwitch
ipSwitch :: (Monad m , Monoid s, Monoid e) =>
           (forall sf. a -> [sf] -> m [(b, sf)]) ->
           [Wire s e m b c] ->
           Wire s e m (a, [c]) (Event evt) ->
           ([Wire s e m b c] ->  evt -> Wire s e m a [c]) ->
           Wire s e m a [c]
ipSwitch route col sig continue  =
  let pSwitch' col sig = mkGen $ \ds a -> do
        col' <- mapM (\(x,w) -> stepWire w ds (Right x)) =<< route a col
        let output =Right $ fmap (\(Right x) -> x) $ filter isRight $ map fst col'
            wires = fmap snd col'
        (eitherEvt,sig') <- stepWire sig ds (fmap (a,) output)
        return ( (,) <$> output <*> (fmap (continue wires) <$> eitherEvt), pSwitch' wires sig' )
  in switch $ pSwitch' col sig

dipSwitch :: (Monad m , Monoid s, Monoid e) =>
           (forall sf. a -> [sf] -> m [(b, sf)]) ->
           [Wire s e m b c] ->
           Wire s e m (a, [c]) (Event evt) ->
           ([Wire s e m b c] ->  evt -> Wire s e m a [c]) ->
           Wire s e m a [c]
dipSwitch route col sig continue  =
  let pSwitch' col sig = mkGen $ \ds a -> do
        col' <- mapM (\(x,w) -> stepWire w ds (Right x)) =<< route a col
        let col'' =  filter (isRight.fst) col'
            output =Right $ fmap (\(Right x) -> x) $ map fst col''
            wires = map snd col''
        (eitherEvt,sig') <- stepWire sig ds (fmap (a,) output)
        return $ ( (,) <$> output <*> (fmap (continue wires) <$> eitherEvt), pSwitch' wires sig' )
  in dSwitch $ pSwitch' col sig 
