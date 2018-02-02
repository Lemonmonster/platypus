{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE MultiParamTypeClasses#-}
{-#LANGUAGE GADTs#-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE Arrows#-}
{-# LANGUAGE Strict #-}

module Graphics where

import Prelude hiding ((.),id)
import SDL.Video hiding (Renderer,viewport)
import Renderer
import Entity
import Wires hiding (when)
import Control.Monad
import Linear.V2
import Input
import Control.Applicative
import System.Mem
import Graphics.Rendering.OpenGL as GL hiding (viewport,scale)
import Shape
import Geometry
import Sprite
import TextBlock
import Linear.V4
import FRP.Netwire.Analyze
import TextBlock
import Geometry
import Control.Lens ((^.))
import Debug.Trace
import Data.Typeable
import Data.Maybe

class Visible a where
  toDrawable :: a -> DrawableObject

drawW :: (Typeable a,HasId a,Visible a,Monad m) =>  Wire s e m ([a],SignalL ls) ([a],SignalL (DrawableObject ': ls))
drawW  = signal_ toDrawable

instance EntityW '[Delayed Renderer, WindowSize, Window] '[DrawableObject] Renderer '[] where
  wire = mkGen $ \s (lst `ECons` size `ECons`win `ECons` ENil,  drawables `SCons` SNil) -> do
    case (size,win) of
      ([WS (V2 w h)],[window] ) -> do
        let drawables' = map _payload drawables
        GL.clear [ColorBuffer,DepthBuffer]
        renderer <- head $ (map (return.fromDelay) lst) <|> [newRenderer 640 320]
        renderer' <- render renderer (realToFrac $ dtime s) drawables'
        --performGC
        glSwapWindow window
        return (Right ([renderer'],SNil),wire )
      _ -> do
        return (Right ([],SNil),wire )

newtype Framerate = Framerate Float

instance EntityW '[Renderer] '[] Framerate '[DrawableObject] where
  wire = proc ([r] `ECons` ENil,_) -> do
    f1 <- lowestOver 1 . (0 >--framerate) -< ()
    f2 <-  hold <<< periodic 0.05 <<<(0 >--framerate) -< ()
    let d = V2 20 60
        p = V2 (-280) (-100)
        sigFig x = fromIntegral (round $ x*100) / 100
    returnA -< ([Framerate f2],[Signal Nothing (DO $ textBox (show (sigFig f1) ++ " " ++show (sigFig f2) ) defaultFont (newOBB p d))] `SCons` SNil)
