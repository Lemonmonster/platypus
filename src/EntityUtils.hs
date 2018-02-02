{-#LANGUAGE MultiParamTypeClasses#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE PolyKinds #-}
{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE UndecidableInstances#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ExplicitForAll#-}
--{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE Arrows #-}
module EntityUtils (mkObject) where

import Entity
import qualified Data.Map as M
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

class SameLength' (es1 :: [k]) (es2 :: [m])
instance (es2 ~ '[]) => SameLength' '[] es2
instance (SameLength' xs ys, es2 ~ (y ': ys)) => SameLength' (x ': xs) es2

class ApplyAB f a b where
  applyAB :: f -> a -> b

class HMap (r1 :: [*]-> *) (r2 :: [*]-> *) f (a::[*]) (b::[*]) where
  hmap ::(SameLength' a b) => f -> r1 a -> r2 b

instance ApplyAB (a -> b) a b where
  applyAB = ($)

instance HMap EntityL HList f '[] '[] where
  hmap _ _ = HNil

instance (ApplyAB f [a] b, HMap EntityL HList f l1 l2,SameLength' l1 l2)=> HMap EntityL HList f (a ': l1) (b ': l2) where
  hmap f (x `ECons` xs) = applyAB f x `HCons` hmap f xs

instance HMap SignalL HList f '[] '[] where
  hmap _ _ = HNil

instance (ApplyAB f [Signal a] b, HMap SignalL HList f l1 l2,SameLength' l1 l2)=> HMap SignalL HList f (a ': l1) (b ': l2) where
  hmap f (x `SCons` xs) = applyAB f x `HCons` hmap f xs

instance HMap HList HList f '[] '[] where
  hmap _ _ = HNil

instance (ApplyAB f a b, HMap HList HList f l1 l2,SameLength' l1 l2)=> HMap HList HList f (a ': l1) (b ': l2) where
  hmap f (x `HCons` xs) = applyAB f x `HCons` hmap f xs

data ToMap = ToMap
data SigToMap = SigToMap
newtype (Ord a) => Lookup a = Lookup a

type family MapType a  where
  MapType (M.Map EntityId a) = 1
  MapType (M.Map (Maybe EntityId) a) = 2
  MapType a = 0

type family MapOut a where
  MapOut (M.Map EntityId a) = (M.Map (Maybe EntityId) a)
  MapOut (M.Map (Maybe EntityId) a) = (M.Map (Maybe EntityId) a)
  MapOut a = (M.Map (Maybe EntityId) a)

instance (ApplyAB' (MapType a) ToMap [a] (MapOut a),MapOut a ~ b) => ApplyAB ToMap [a] b where
  applyAB ToMap l = applyAB' (Proxy :: Proxy (MapType a)) ToMap l

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

instance (Ord a) => ApplyAB (Lookup (Maybe a)) (M.Map (Maybe a) b) b where
  applyAB (Lookup x) m =
    case M.lookup x m  of
      Just a -> a
      Nothing -> case M.lookup Nothing m of
                    Just a -> a
                    Nothing -> error "failed lookup of object component"

class Contains a (xs :: [*])
instance Contains a (a ': xs)
instance (Contains a xs) => Contains a (b ': xs)

class Sublist (a :: [*]) (b :: [*])
instance Sublist '[] b
instance (Contains a b,Sublist as b) => Sublist (a ': as) b

class  FindH (r :: [*] -> *) a (lst :: [*]) where
  findH :: r lst -> a

type family TEq a b :: Bool where
  TEq a a = 'True
  TEq a b = 'False

instance  (TEq a b ~ f, FindH' HList f a (b ': lst))=>FindH HList a (b ': lst) where
  findH = findH' (Proxy :: Proxy f)

class FindH' (r::[*] -> *) (b :: Bool) a (l :: [*]) where
  findH' :: Proxy b -> r l -> a

instance FindH' HList 'True a (a ': lst)  where
  findH' Proxy (x `HCons` _) = x

instance (TEq a b ~ f,FindH' HList f a (c ': lst))=>FindH' HList 'False a (b ': c ': lst)  where
  findH' Proxy (_ `HCons` lst) = findH' (Proxy :: Proxy f) lst

instance  (TEq a b ~ f, FindH' EntityL f [a] (b ': lst))=>FindH EntityL [a] (b ': lst) where
  findH = findH' (Proxy :: Proxy f)

instance FindH' EntityL 'True [a] (a ': lst)  where
  findH' Proxy (x `ECons` _) = x

instance (TEq a b ~ f,FindH' EntityL f [a] (c ': lst))=>FindH' EntityL 'False [a] (b ': c ': lst)  where
  findH' Proxy (_ `ECons` lst) = findH' (Proxy :: Proxy f) lst

class  Extract (r::[*] -> *) (a :: [*]) (b :: [*]) where
  extract :: r a -> r b

instance (FindH HList b lst ,Sublist lst (b ': bs), Extract HList lst bs) => Extract HList lst (b ': bs) where
  extract lst = findH lst `HCons` extract lst

instance Extract HList lst '[] where
  extract _ = HNil




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
  UnmapArgs (M.Map (Maybe EntityId) a ': as) =  a ':UnmapArgs as
  UnmapArgs (M.Map EntityId a ': as) =  a ': UnmapArgs as
  UnmapArgs (a ': as) = a ': UnmapArgs as
  UnmapArgs '[] = '[]

data EvtWrapper a = C a | Id  (EntityId -> a)

mkObject :: forall s e ein sin a sout . (Typeable a, HasId a, Monoid s,
              HMap EntityL HList ToMap ein (MapArgs ein),
              HMap SignalL HList SigToMap sin (MapArgs sin),
              SameLength' ein (MapArgs ein),
              SameLength' sin (MapArgs sin),
              Monoid (SignalL sin),
              Monoid (EntityL ein),
              Monoid (EntityL (UnmapArgs ein)),
              Monoid (SignalL sout),
              HMap HList HList (Lookup (Maybe EntityId)) (MapArgs ein) (UnmapArgs ein),
              HMap HList HList (Lookup (Maybe EntityId)) (MapArgs sin) sin,
              SameLength' (MapArgs ein) (UnmapArgs ein),
              SameLength' (MapArgs sin) sin
              )
               =>
            (a -> Wire s e IO (HList (UnmapArgs ein),HList sin) (a,SignalL sout)) ->
            (EntityId -> SignalL sout) ->
            Wire s e IO (EntityL ein,SignalL ((Create a) ': Destroy ': sin)) ([a],SignalL sout)
mkObject create destroy =
  let mkObject' i start mp news =  mkGen $ \ ds (entIn, creates `SCons` destroys `SCons` sigIn) -> do
        let emlst = hmap ToMap entIn :: HList (MapArgs ein)
            smlst = hmap SigToMap sigIn :: HList (MapArgs sin)
            (updates,newEvents) = partition
              (\ x -> case x of
                (Update a) -> M.member (a^?!ident) mp
                _-> False
                ) $ map _payload creates
            mp' = foldl (\m (Update a) -> m & at (a^?!ident) .~ Just (create a)) mp updates
            toEvent (Create a) = fmap C a
            toEvent (CNoId a) = fmap Id a
            toEvent (Update a) = Event $ C a
            newObjects =   fmap ( (\(a,b) -> (a,Right (map snd b,mempty),M.fromList $ map fst b,mempty)) .
                                  mapAccumL (\i c ->  case c of
                                          (C a) -> (i,((a ^?!ident,create a),a))
                                          (Id f) -> -- assign new id
                                            let newId = (typeRep (Proxy :: Proxy a), i)
                                            in (i+1, ((newId,create $ f newId),f newId))
                                    ) i ) $
                                mconcat $  map (fmap pure.toEvent) newEvents
            removeObjects =
              fmap (  (\(o,m) -> (i,Right ([],o),mempty,M.fromList m)) .
                      foldl (\(res,lst) i ->
                        ( destroy i  <> res,
                          (i,mp^?!at i._Just) : lst)
                            )
                        (mempty,[])
                    ) $ mconcat $ map (fmap (: []).(\(Dest i)-> i)._payload) destroys
        (wireUpdate,output) <- runStateT (mapM (\(oId,oWire) -> do
              let einput = hmap (Lookup (Just oId)) emlst :: HList (UnmapArgs ein)
                  sinput = hmap (Lookup (Just oId)) smlst :: HList sin
              (out,newWire) <-lift $ stepWire oWire ds (Right (einput,sinput))
              modify (either Left (\x->fmap (\(ent,sigs) -> x <> ([ent],sigs)) out))
              return (oId,newWire)
            ) (M.toList mp') ) (Right (mempty,mempty))
        let mp'' = M.fromList  wireUpdate
        return (fmap (,fmap ((\(i,start,nextMap,news)-> switch (mkObject' i start nextMap news . eventFilter) ) )
                        (fmap (\(idCount,sig,adds,deletes) -> (
                                    idCount,
                                    (<>) <$> sig <*> output,
                                    (mp'' `M.difference` deletes),
                                    adds
                                    )) $
                        merge (\(idCount,addSig,adds,_) (_,deleteSig,_,deletes) ->  (
                                idCount,
                                (<>) <$> addSig <*> deleteSig,
                                adds,
                                deletes
                                )) newObjects removeObjects ))
                          ((<>) <$> start <*> output),
                    mkObject' i (Right mempty) (mp'' `M.union` news) M.empty)
      eventFilter = proc (entIn, creates `SCons` destroys `SCons` sigIn) -> do
       c <- dSwitch (pure [] &&& now . pure id) -< map _payload creates
       d <- dSwitch (pure [] &&& now . pure id) -<  map _payload destroys
       returnA -< (entIn,sigify c `SCons` sigify d `SCons` sigIn)

  in switch $ mkObject' 0 (Right mempty) M.empty M.empty
