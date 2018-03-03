{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Graphics where

import Control.Applicative
import Control.Lens ((^.))
import Control.Monad
import Data.Maybe
import Data.Typeable
import Debug.Trace
import Entity
import FRP.Netwire.Analyze
import Geometry
import Graphics.Rendering.OpenGL as GL hiding (scale, viewport)
import Input
import Linear.V2
import Linear.V4
import Prelude hiding ((.), id)
import Renderer
import SDL.Video hiding (Renderer, viewport)
import Shape
import Sprite
import System.Mem
import TextBlock
import TextBlock
import Wires hiding (when)

red :: V4 Float
red = V4 1 0 0 1

green :: V4 Float
green = V4 0 1 0 1

blue :: V4 Float
blue = V4 0 0 1 1

yellow :: V4 Float
yellow = V4 1 1 0 1

magenta :: V4 Float
magenta = V4 1 0 1 1

cyan :: V4 Float
cyan = V4 0 1 1 1

white :: V4 Float
white = 1

black :: V4 Float
black = V4 0 0 0 1

class Visible a where
  toDrawableObjects :: a -> [DrawableObject]
  default toDrawableObjects :: (Drawable a) => a -> [DrawableObject]
  toDrawableObjects = pure.tdo

tdos :: (Visible a) => a -> [DrawableObject]
tdos = toDrawableObjects

drawW :: (Typeable a,HasId a,Visible a,Monad m) =>  Wire s e m ([a],SignalL ls) ([a],SignalL (DrawableObject ': ls))
drawW  = signalConcat_ tdos


instance EntityW '[Delayed Renderer, WindowSize, Window] '[DrawableObject] Renderer '[] where
  wire = mkGen $ \s (lst `ECons` size `ECons`win `ECons` ENil,  drawables `SCons` SNil) ->
    case (size,win) of
      ([WS (V2 w h)],[window] ) -> do
        let drawables' = map _payload drawables
        GL.clear [ColorBuffer,DepthBuffer]
        renderer <- head $ (map (return.fromDelay) lst) <|> [newRenderer 640 320]
        renderer' <- render renderer (realToFrac $ dtime s) drawables'
        --performGC
        glSwapWindow window
        return (Right ([renderer'],SNil),wire )
      _ ->
        return (Right ([],SNil),wire )

newtype Framerate = Framerate Float

instance EntityW '[Renderer] '[] Framerate '[DrawableObject] where
  wire = proc ([r] `ECons` ENil,_) -> do
    f1 <- lowestOver 1 . (0 >--framerate) -< ()
    f2 <-  hold <<< periodic 0.05 <<<(0 >--framerate) -< ()
    let d = V2 20 60
        p = V2 (-280) (-100)
        sigFig x = fromIntegral (round $ x*100) / 100
    returnA -< ([Framerate f2],[Signal Nothing (tdo $ textBox (show (sigFig f1) ++ " " ++show (sigFig f2) ) defaultFont (newOBB p d))] `SCons` SNil)

