{-#LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances, FlexibleContexts,
            UndecidableInstances, DataKinds, PolyKinds, ScopedTypeVariables,
            FunctionalDependencies, RankNTypes, TypeOperators #-}
{- This module consists of reimplementatins and modifications of things from the HList package (https://hackage.haskell.org/package/HList) .
   I was having difficulty getting it to work with my dependencies when I started this project.  It has since been updated to
   work with modern GHC
-}
module HList where

import Control.Lens hiding (Contains)
import Data.Typeable
import Control.DeepSeq
import Data.Monoid (( <> ))

type family TEq a b :: Bool where
  TEq a a = 'True
  TEq a b = 'False

data family Id a

newtype EntityId = EntityId (TypeRep,Int) deriving (Show)

idType :: EntityId -> TypeRep
idType (EntityId (t,_)) = t

idVal :: EntityId -> Int
idVal (EntityId (_,i)) = i

instance Eq EntityId where
  (EntityId (t1,v1)) == (EntityId (t2,v2)) =
    (t1 == t2) && ((v1 == v2) || (v1<=0 && v2<=0))

instance Ord EntityId where
  compare (EntityId (t1,v1)) (EntityId (t2,v2)) =
         compare t1 t2 <>
             if (v1<=0 && v2<=0)
                then EQ
                else compare v1 v2

instance NFData EntityId where
  rnf d@(EntityId val) = d `seq` rnf val


data Signal c = Signal{_target::Maybe EntityId,_payload::c}
makeLenses ''Signal

instance Functor Signal where
  fmap f s@(Signal _ p) = s{_payload= f p}

sigify :: [a] -> [Signal a]
sigify = map $ Signal Nothing

instance (Show a) => Show (Signal a) where
  show (Signal x y) = "(Signal " ++ show x ++" "++show y++")"

class Headable (r :: [*] -> *)  (ls :: [*]) a | r ls -> a where
  headH :: r ls -> a

class Tailable (r :: [*] -> *) (l :: [*]) (ls :: [*]) | r l -> ls where
  tailH :: r l -> r ls

class TypeIxable (r :: [*] -> *) (l :: [*]) a where
  tIx :: Traversal' (r l) a
  tIxp :: Proxy a -> Traversal' (r l) a
  tIxp p = tIx

class Nilable (r :: [*] -> *) where
  nil' :: r '[]

class Consable (r :: [*] -> *) l ls a | r a l -> ls where
  cons' :: a -> r l -> r ls
  (.*.) :: a -> r l -> r ls
  (.*.) = cons'

infixr 6 `cons'`
infixr 6 .*.

class HBuild' (r :: [*] -> * ) (l :: [*]) o where
  hBuild' :: r l -> o

instance HBuild' r l (r l) where
  hBuild' = id

instance (Consable r l ls a,HBuild' r ls x) => HBuild' r l ( a -> x) where
  hBuild' lst x = hBuild' (cons' x lst)

class TypeIxable' (r :: [*] -> * ) (l :: [*]) a (e :: Bool) where
  tIx' :: Proxy e -> Traversal' (r l) a

instance (Nilable r) => TypeIxable r '[] a where
  tIx f r = pure nil'

instance (Headable r (b ': l) h, TypeIxable' r (b ': l) a (TEq a h)) =>TypeIxable r (b ': l) a where
  tIx = tIx' (Proxy :: Proxy (TEq a h))

instance (Consable r l ls a, Headable r ls a, Tailable r ls l , TypeIxable r l a) => TypeIxable' r ls a False where
  tIx' _ f lst  = cons' (headH lst) <$> tIx f (tailH lst)

instance (Consable r l ls a, Headable r ls a, Tailable r ls l , TypeIxable r l a) => TypeIxable' r ls a True where
  tIx' _ f lst  = cons' <$> f (headH lst) <*> tIx f (tailH lst)

data family HList (l::[*])

data instance HList '[] = HNil
data instance HList (x ': xs) = x `HCons` HList xs


hBuild :: (HBuild' HList '[] o) => o
hBuild = hBuild' HNil

hEnd :: HList l -> HList l
hEnd = id

infixr 8 `HCons`

instance Headable HList (a ': b) a where
  headH (HCons x l) = x

instance Tailable HList (a ': b) b where
  tailH (HCons x l) = l

instance Nilable HList where
  nil' = HNil

instance Consable HList l (a ': l) a where
  cons' = HCons

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show a,Show (HList b)) => Show (HList (a ': b)) where
  show (HCons x l) = "(HCons "++show x++" "++show l++")"


data family SignalL (l::[*])

data instance SignalL '[] = SNil
data instance SignalL (x ': xs) = [Signal x] `SCons` SignalL xs

infixr 8 `SCons`

instance Headable SignalL (a ': b) [Signal a] where
  headH (SCons x l) = x

instance Tailable SignalL (a ': b) b where
  tailH (SCons x l) = l

instance Nilable SignalL where
  nil' = SNil

instance Consable SignalL l (a ': l) [Signal a] where
  cons' = SCons

instance Show (SignalL '[]) where
  show SNil = "SNil"

instance (Show a,Show (SignalL b)) => Show (SignalL (a ': b)) where
  show (SCons x l) = "(SCons "++show x++" "++show l++")"

class EmptySig (a::[*]) where
  emptySignal :: SignalL a

instance EmptySig c => EmptySig ( a  ': c) where
  emptySignal = [] `SCons` emptySignal

instance EmptySig '[] where
  emptySignal = SNil


data family EntityL (l::[*])

data instance EntityL '[] = ENil
data instance EntityL (x ': xs) = [x] `ECons` EntityL xs

infixr 8 `ECons`

instance Headable EntityL (a ': b) [a] where
  headH (ECons x l) = x

instance Tailable EntityL (a ': b) b where
  tailH (ECons x l) = l

instance Nilable EntityL where
  nil' = ENil

instance Consable EntityL l (a ': l) [ a] where
  cons' = ECons

instance Show (EntityL '[]) where
  show ENil = "ENil"

instance (Show a,Show (EntityL b)) => Show (EntityL (a ': b)) where
  show (ECons x l) = "(ECons "++show x++" "++show l++")"

class EmptyEnt (a::[*]) where
  emptyEntity :: EntityL a

instance (EmptyEnt c) => EmptyEnt (a ': c) where
  emptyEntity = [] `ECons` emptyEntity

instance EmptyEnt '[] where
  emptyEntity = ENil

class SameLength' (es1 :: [k]) (es2 :: [m])
instance (es2 ~ '[]) => SameLength' '[] es2
instance (SameLength' xs ys, es2 ~ (y ': ys)) => SameLength' (x ': xs) es2

class ApplyAB f a b | f a -> b where
  applyAB :: f -> a -> b

class HMap (r1 :: [*]-> *) (r2 :: [*]-> *) f (a::[*]) (b::[*]) | r1 r2 f a -> b where
  hmap ::(SameLength' a b) => f -> r1 a -> r2 b

class HFoldr (r :: [*] -> *) f (l :: [*]) v a | r l v f -> a where
  hFoldr :: f -> v -> r l -> a

class HFoldl (r :: [*] -> *) f (l :: [*]) v a | r l v f -> a where
  hFoldl :: f -> v ->  r l -> a

instance ApplyAB (a -> b) a b where
  applyAB = ($)

instance (Nilable b) => HMap a b f '[] '[] where
  hmap _ _ = nil'

instance (ApplyAB f a b,
          HMap r1 r2 f l1 l2,
          SameLength' l1 l2,
          Headable r1 (e1 ': l1)  a ,
          Consable r2 l2 (e2 ': l2) b,
          Tailable r1 (e1 ': l1)  l1) => HMap r1 r2 f (e1 ': l1) (e2 ': l2) where
   hmap f lst = applyAB f (headH lst) `cons'` hmap f (tailH lst)

instance (a ~ b) => HFoldl r f '[] a b where
  hFoldl _ = const

instance (ApplyAB f (b,h) a,
          Headable r l h,
          Tailable r l ls,
          HFoldr r f ls v b) => HFoldr r f l v a where
  hFoldr f v ls = applyAB f  (hFoldr f v (tailH ls),headH ls)

instance (a ~ b) => HFoldr r f '[] a b  where
  hFoldr _ = const

instance (zx ~ (v,h),
          ApplyAB f zx b,
          Headable r (e ': ls) h,
          Tailable r (e ': ls) ls,
          HFoldl r f ls b a) => HFoldl r f (e ': ls) v a where
  hFoldl f v ls = hFoldl f (applyAB f (v,headH ls)) (tailH ls)
