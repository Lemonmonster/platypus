{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll #-}
--{-#LANGUAGE TemplateHaskell#-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# Language DefaultSignatures #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language Strict #-}
{-# Language StrictData #-}
module EntityUtils (
  mkObject,
  Manager,
  managedIds
 ) where

import Entity
import qualified Data.Map.Strict as M
import Data.Typeable
import Control.Lens hiding (Contains)
import Signals
import Prelude hiding ((.),id)
import Control.Wire hiding ((<>),at,when)
import Control.Monad.State
import Data.Maybe
import Data.Either
import Data.Monoid
import qualified Data.Semigroup as S
import GHC.TypeLits hiding (type (+), type (-),type (^),type (*))
import Debug.Trace
import Data.List
import Control.Wire.Unsafe.Event
import Control.Monad (when)
--recreates a lot of the functions from the HList package



data ToMap = ToMap
data SigToMap = SigToMap
newtype Lookup a = Lookup [a]

type family MapType a  where
  MapType (M.Map EntityId a) = 1
  MapType (M.Map (Maybe EntityId) a) = 2
  MapType a = 0

type family MapOut a where
  MapOut (M.Map EntityId a) = (M.Map (Maybe EntityId) a)
  MapOut (M.Map (Maybe EntityId) a) = (M.Map (Maybe EntityId) a)
  MapOut a = (M.Map (Maybe EntityId) a)

instance (ApplyAB' (MapType a) ToMap [a] (MapOut a),MapOut a ~ b) => ApplyAB ToMap [a] b where
  applyAB ToMap = applyAB' (Proxy :: Proxy (MapType a)) ToMap

class ApplyAB' (flag :: Nat) f a b where
  applyAB' :: Proxy flag -> f -> a -> b

instance (HasId a) => ApplyAB' 0 ToMap [a] (M.Map (Maybe EntityId) a) where
  applyAB' Proxy ToMap l = M.fromList $ map (\x ->(_ident x,x)) l

instance ApplyAB' 1 ToMap [M.Map EntityId a] (M.Map (Maybe EntityId) a) where
  applyAB' Proxy ToMap (m:_) = M.mapKeys Just m

instance ApplyAB' 2 ToMap [M.Map (Maybe EntityId) a] (M.Map (Maybe EntityId) a) where
  applyAB' Proxy ToMap = head

instance ApplyAB SigToMap [Signal a] (M.Map (Maybe EntityId) a) where
  applyAB SigToMap l = M.fromList $ map (\(Signal i d) ->(i,d)) l

instance (Ord a) => ApplyAB (Lookup a) (M.Map (Maybe a) b) [b] where
  applyAB (Lookup x) m =
    let l = mapMaybe ((`M.lookup` m).Just) x
    in  if null l then
          maybeToList $ m^?at Nothing ._Just
        else
          l



type family IsFunc a where
  IsFunc (a->b) = 'True
  IsFunc a = 'False

type family ArgList f :: [*] where
  ArgList (a->b) = a ': ArgList b
  ArgList a = '[]

{-class  HApplier (r :: [*] -> *) a (lst :: [*]) b where
  applyToH :: a -> r lst -> b

class HApplier' (r :: [*] -> *) (flag::Bool) a (lst :: [*]) b where
  applyToH' :: Proxy flag-> a -> r lst -> b

instance (IsFunc a ~ f,HApplier' r f a lst b) => HApplier (r :: [*] -> *) a (lst :: [*]) b where
  applyToH = applyToH'  (Proxy :: Proxy f)

instance HApplier' r False b lst b where
  applyToH' Proxy b _ = b

instance (IsFunc d ~ f,HApplier' r f d lst e) => HApplier' r True (r lst->d) lst e where
  applyToH' Proxy a lst = applyToH' (Proxy :: Proxy f) (a lst) lst

type family LookupArgs x (a::[*]) where
  LookupArgs x (a ': as ) = M.Map x a ': LookupArgs x as
  LookupArgs x '[] = '[]

type family ToMapArgs (a::[*]) where
  ToMapArgs (M.Map a b ': xs) = b ': ToMapArgs xs
  ToMapArgs '[] = '[]-}

{-getObjs :: forall f es a.
     (SameLength' (ToMapArgs (LookupArgs EntityId (ArgList f))) (LookupArgs EntityId (ArgList f)),
      SameLength' (LookupArgs EntityId (ArgList f))  (ArgList f),
      HMap HList HList (Lookup EntityId) (LookupArgs EntityId (ArgList f)) (ArgList f),
      HMap EntityL HList ToMap (ToMapArgs (LookupArgs EntityId (ArgList f))) (LookupArgs EntityId (ArgList f)),
      Extract EntityL es (ToMapArgs (LookupArgs EntityId (ArgList f))),
      HApplier HList f (ArgList f) a) =>
   f ->  EntityL es -> [a]
getObjs app lst =
  let a = extract lst :: EntityL (ToMapArgs (LookupArgs EntityId (ArgList f)))
      b = hmap ToMap a :: HList (LookupArgs EntityId (ArgList f))
      ids = map fst $ M.toList $ headH b
  in map (\x->applyToH app $ (hmap (Lookup x) b)) ids-}


instance (EmptyEnt b,Monoid (EntityL b)) => Monoid (EntityL (a ': b)) where
  mempty = emptyEntity
  mappend (x `ECons` xs) (y `ECons` ys) = (x ++ y) `ECons` mappend xs ys

instance Monoid (EntityL '[]) where
  mempty = emptyEntity
  mappend ENil ENil = ENil

instance (EmptySig b,Monoid (SignalL b)) => Monoid (SignalL (a ': b)) where
  mempty = emptySignal
  mappend (x `SCons` xs) (y `SCons` ys) = (x ++ y) `SCons` mappend xs ys

instance Monoid (SignalL '[]) where
  mempty = emptySignal
  mappend SNil SNil = SNil

type family MapArgs (a :: [*]) where
  MapArgs (M.Map (Maybe EntityId) a ': as) = M.Map (Maybe EntityId) a ': MapArgs as
  MapArgs (M.Map EntityId a ': as) = M.Map (Maybe EntityId) a ': MapArgs as
  MapArgs (a ': as) = M.Map (Maybe EntityId) a ': MapArgs as
  MapArgs '[] = '[]

{-data IDTest a = IDTest {_eident::EntityId,_dat::a} deriving (Typeable)
makeLenses 'IDTest

instance HasId (IDTest a) where
  ident = eident

idTestFloat = typeRep (Proxy :: Proxy (IDTest Float))
idTestInt = typeRep (Proxy :: Proxy (IDTest Int))
-}
--mapTest = hmap ToMap $ [IDTest (idTestFloat,0) (5::Float)] `ECons` [IDTest (idTestInt,0) (5::Int)] `ECons` ENil :: HList (MapArgs [IDTest Float, IDTest Int])

--instance SameLength' a a


type family UnmapArgs (a:: [*]) where
  UnmapArgs (M.Map (Maybe EntityId) a ': as) =  [a] ':UnmapArgs as
  UnmapArgs (M.Map EntityId a ': as) =  [a] ': UnmapArgs as
  UnmapArgs (a ': as) = [a] ': UnmapArgs as
  UnmapArgs '[] = '[]

data EvtWrapper a = C a | Id  (EntityId -> a)

class Manager a where
  managedIds :: a -> [EntityId]

  default managedIds :: (HasId a) => a -> [EntityId]
  managedIds a = maybeToList $ a^?ident

--hiding constructor
newtype EntIndex = EntIndex Int
type ObjectMonad a = StateT EntIndex IO

getIds :: forall a s e . (Typeable a,Monoid e) => Int -> Wire s e (ObjectMonad a) (Event Int) [EntityId] -- creates the passed number of ids, events create further Ids
getIds x =
  proc e -> do
    evt <- now -< x
    rec ents <- delay [] <<< rSwitch (mkConst $ Right []) -< (ents,
                            (\x -> mkGen_ (\ents -> do
                                            (EntIndex i) <- get
                                            put (EntIndex $ i + x)
                                            return $ Right (ents ++ map (\x -> EntityId (typeRep (Proxy :: Proxy a),x)) [i..i+x])
                            ) >>> now >>> hold)
                          <$> mergeL evt e)
    returnA -< ents


mkObject :: forall s e ein sin a sout . (Typeable a, HasId a,Manager a, Monoid s,
              HMap EntityL HList ToMap ein (MapArgs ein),
              HMap SignalL HList SigToMap sin (MapArgs sin),
              SameLength' ein (MapArgs ein),
              SameLength' sin (MapArgs sin),
              Monoid (SignalL sin),
              Monoid (EntityL ein),
              Monoid (EntityL (UnmapArgs ein)),
              Monoid (SignalL sout),
              HMap HList HList (Lookup EntityId) (MapArgs ein) (UnmapArgs ein),
              HMap HList HList (Lookup EntityId) (MapArgs sin) sin,
              SameLength' (MapArgs ein) (UnmapArgs ein),
              SameLength' (MapArgs sin) sin
              )
               =>
            (a -> Wire s e (ObjectMonad a) (HList (UnmapArgs ein),HList sin) (a,SignalL sout)) ->
            (EntityId -> SignalL sout) ->
            Wire s e IO (EntityL ein,SignalL ((Create a) ': Destroy ': sin)) ([a],SignalL sout)
mkObject create destroy =
  let mkObject' i mp assocIds =  mkGen $ \ ds (entIn, creates `SCons` destroys `SCons` sigIn) -> do
        let emlst = hmap ToMap entIn :: HList (MapArgs ein)
            smlst = hmap SigToMap sigIn :: HList (MapArgs sin)
            (updates,newEvents) = partition
              (\case
                (Update a) -> M.member (a^?!ident) mp
                _-> False
                ) $ map _payload creates
            mp' = foldl (\mp d -> case d of
                    (Dest (Event i)) -> M.delete i mp
                    _ -> mp
                ) (foldl (\m (Update a) -> m & at (a^?!ident) .~ Just (create a)) mp updates) (map _payload destroys)
        (wireUpdate,(output,i')) <- runStateT (mapM (\(oId,oWire) -> do
              let einput = hmap (Lookup (assocIds^?!at oId._Just)) emlst :: HList (UnmapArgs ein)
                  sinput = hmap (Lookup (assocIds^?!at oId._Just)) smlst :: HList sin
              ((out,newWire),EntIndex id') <-lift $ runStateT (stepWire oWire ds (Right (einput,sinput))) (EntIndex i)
              _1 %= either Left (\x->fmap (\(ent,sigs) -> x <> ([ent],sigs)) out)
              return (oId,newWire)
            ) (M.toList mp') ) (Right (mempty,mempty),i)
        let (news,i'') = foldl' (\(lst,i) c -> case c of
                (Update a) |  M.member (a^?!ident) mp -> (lst,i)
                           | otherwise -> ((a^?!ident,a):lst,i)
                (Create (Event a)) -> ((a^?!ident,a):lst,i)
                (CNoId (Event f)) ->
                  let id = EntityId (typeRep (Proxy :: Proxy a),i)
                  in  ((id, f id):lst ,i+1)
                _ -> (lst,i)
              ) ([],i') $ map _payload creates
            mp'' = M.fromList $ foldl' (\mp (i,a) ->
                  (i,create a):mp
                ) wireUpdate news
            mpOut = foldl (\mp d -> case d of
                    (Dest (Event i)) -> M.delete i mp
                    _ -> mp
                ) mp'' (map _payload destroys)
        return (fmap (\(ents,sigs) -> (ents ++ map snd news,sigs)) output,
                    mkObject' i'' mpOut $
                        either (const (assocIds :: M.Map EntityId [EntityId]))
                          (foldl (\mp x -> mp & at (x^?!ident) .~ Just (managedIds x) ) assocIds.(++ map snd news).fst) output
                       )
  in mkObject' 0 M.empty M.empty
