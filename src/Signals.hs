{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE FlexibleInstances #-}
module Signals where

import Control.Wire.Unsafe.Event (Event(..))
import Entity
import Control.DeepSeq

--pulls double duty for creating and updating events
data Create a = Create (Event a) | CNoId (Event (EntityId -> a)) | Update a

newtype Destroy = Dest (Event EntityId)  

instance NFData Destroy where
  rnf d@(Dest a) = d `seq` rnf a

class SignalType s t | s -> t where
  unsignal:: [Signal s] -> [t]

instance SignalType (Create a) a where
  unsignal = map (\(Signal _ (Update a)) -> a)

instance (Show a) => Show (Create a) where
  show (Create (Event a)) = "(Create (Event " ++ show a ++"))"
  show (CNoId _) = "(CNoId Event)"
  show (Update a) = "(Update " ++ show a ++ ")"


instance Show Destroy where
  show (Dest NoEvent) = "(Dest NoEvent)"
  show (Dest (Event a))= "(Dest (Event " ++ show a ++ "))"

instance (Show a) => Show ( Event a ) where
  show (Event a) = "(Event " ++ show a ++ ")"
  show NoEvent = "NoEvent"
