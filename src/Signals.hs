{-#LANGUAGE MultiParamTypeClasses#-}
{-#LANGUAGE FunctionalDependencies#-}
{-#LANGUAGE FlexibleInstances#-}
module Signals where

import Control.Wire (Event)
import Entity

--pulls double duty for creating and updating events
data Create a = Create (Event a) | CNoId (Event (EntityId -> a)) | Update a

newtype Destroy = Dest (Event EntityId)

class SignalType s t | s -> t where
  unsignal:: [Signal s] -> [t]

instance SignalType (Create a) a where
  unsignal = map (\(Signal _ (Update a)) -> a)
