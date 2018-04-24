{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics where

import Control.Applicative
import Control.Lens ((^.),makeLenses,(&),(.~))
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
import Linear ((^*))
import Prelude hiding ((.), id)
import Renderer
import SDL.Video hiding (Renderer, viewport)
import Shape
import Sprite
import Signals
import System.Mem
import TextBlock
import TextBlock
import Wires hiding (when)
import Data.List (find)

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

data Trackable = Trackable{
    _tid :: EntityId,
    _tShape :: PhysShape
}
makeLenses ''Trackable
newtype CameraEvent = Focus (Event EntityId) deriving (Show)
newtype Camera = Camera Transform deriving (Eq,Ord,Show)

instance EntityW '[Renderer] '[Trackable,CameraEvent] Camera '[] where
  wire = switch $ proc ([renderer] `ECons` ENil,_) -> do
    evt <- ( arr $ fmap (\startRenderer ->
         let (Transform spos sscale sorient) = startRenderer^.viewport.trans
             spring target curr =  (target - curr)*10
         in proc ([currRenderer] `ECons` ENil ,trackables `SCons` evts `SCons` SNil) -> do
               track <- rSwitch $ mkConst (Right Nothing) -<
                  ((), mkConst . Right . Just <$> signalToEvent  (\(Focus id)-> Just id) const evts )
               let (Just target) = (case track of
                               Just t -> (^.tShape.trans) <$> find (\t1 -> t1^.tid == t) (map _payload trackables)
                               Nothing -> Nothing
                            )
                                <|>
                            Just (currRenderer^.viewport.trans)
               rec posO <- integral spos . arr (uncurry spring) . delay (spos,spos)  -< (target^.translation,posO)
               rec scaleO <- integral (sscale^._y) . arr (uncurry spring) . delay (sscale^._y,sscale^._y)  -< (target^.scale._y,scaleO)
               rec orientO <- integral sorient . arr (uncurry spring) . delay (sorient,sorient)  -< (target^.orientation,orientO)
               returnA -<  ([Camera (Transform posO (sscale ^* (scaleO/sscale^._y)) orientO)],SNil)
              )) . now -< renderer
    returnA -< (([],SNil),evt)



instance EntityW '[Delayed Renderer, WindowSize, Window, Delayed Camera] '[DrawableObject] Renderer '[] where
  wire = mkGen $ \s (lst `ECons` size `ECons`win `ECons` cam `ECons` ENil,  drawables `SCons` SNil) ->
    case (size,win) of
      ([WS (V2 w h)],[window] ) -> do
        let drawables' = map _payload drawables
        GL.clear [ColorBuffer,DepthBuffer]
        renderer <- head $ (map (return.fromDelay) lst) <|> [newRenderer 640 320]
        let renderer' = case cam of
                         [D (Camera t)] -> renderer & viewport.trans .~ t
                         _ -> renderer
        renderer'' <- render renderer' (realToFrac $ dtime s) drawables'
        --performGC
        glSwapWindow window
        return (Right ([renderer''],SNil),wire )
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

