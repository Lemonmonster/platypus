{-#LANGUAGE MultiParamTypeClasses#-}
{-#LANGUAGE FunctionalDependencies#-}
{-#LANGUAGE FlexibleInstances#-}
module Signals where

import Entity

newtype Create a = C a
newtype Destroy = Destroy EntityId

class SignalType s t | s -> t where
  unsignal:: [Signal s] -> [t]

instance SignalType (Create a) a where
  unsignal = map (\(Signal _ (C a)) -> a)
