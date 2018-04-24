{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE DataKinds#-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE Strict#-}
module Network where
import Prelude hiding ((.), id)
import Control.Wire hiding (at,when,unless)
import Entity
import TestEntity
import Barrell
import Graphics
import Input
import SDL hiding (delay)
import Physics
import BasicGeometry
import DefenderShip
import SceneManager
import Barrell

network = $(generateNetwork)

{-w1 = wire :: Wire (Timed NominalDiffTime ()) () IO  (EntityL '[(Window,GLContext)],SignalL '[]) ([SDL.Event],SignalL '[Quit])

w1' = arr (\(x) -> (x `ECons` ENil,SNil)) >>> w1 >>> arr (\(e,q `SCons` SNil)-> (e,q))


w2 = wire :: Wire (Timed NominalDiffTime ()) () IO  (EntityL '[Delayed (Window,GLContext)],SignalL '[Quit]) ([(Window,GLContext)],SignalL '[])

w2' = arr (\(x,q) -> (map D x `ECons` ENil,  q `SCons` SNil)) >>> w2  >>> arr (\(x,SNil) -> (x))

network = loop (arr (\(x,b) -> b) >>> delay ([],[]) >>> w2' >>> arr (\x -> (x,x))  >>> second w1'  >>> arr (\(x,(y,z)) -> (x,y,z)) >>> arr (\(x,y,z)-> ((x,y,z),(x,z))))
-}
