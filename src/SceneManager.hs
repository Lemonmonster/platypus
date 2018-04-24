module SceneManager where

import Entity
import Graphics
import Background
import Geometry
import Renderer
import Control.Arrow
import Linear.V2
import Linear.V4
import Barrell
import Signals
import Wires

data SceneEvent = Save String | Load String

data Scene = Scene String

testBackground = 
  Background "Defender" [
     Layer [
         Cell "mountains" 0 0 0 0
      ]
      (V2 512 256)
      (0,256)
      (-2),
       Layer [
         Cell "blank" 0 (V4 0 0.5 0 1) 1 0
       ]
       512
       (-1024,0)
       (-2),
     Layer [
         Cell "blank" 0 (V4 0 1 0 1) 1 0
       ]
       512
       (-1024,0)
       (1),
     Layer [
         Cell "blank" 0 (V4 0.5 0.5 1 1) 1 0
       ]
       512
       (-2048,2048)
       (-1/0)
  ] (newOBB 0 0)

instance EntityW '[Camera] '[] Scene '[DrawableObject,Create Barrell] where
  wire = proc ([Camera t] `ECons` _,_) -> do
    background <- backgroundW testBackground -< OBB t
    barrell <- now -< Barrell (300) 0
    returnA -< ([Scene "not yet implemented"], sigify [background] `SCons` sigify [CNoId barrell] `SCons` SNil)
    
