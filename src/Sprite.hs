{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
module Sprite where

import Renderer
import Control.Lens hiding (Getter)
import Graphics.Rendering.OpenGL hiding (color,Color,get)
import Linear (V2(..),V4(..),M44,_z,_w)
import Geometry
import Atlas
import Control.Monad
import Control.Monad.Fix
import Data.Maybe
import Data.Vector as V
import Data.Map as Map
import Data.Map.Lens
import Control.Lens.At
import Foreign.Marshal.Utils
import Foreign.Ptr
import Data.IORef
import System.IO.Unsafe
import Data.Word
import Data.Bool
import Prelude hiding ((.),id)
import Control.Wire hiding (at)
import Debug.Trace
import Structures


type Color = V4 Float
data Sprite = Sprite{
    _sShape::PhysShape,
    _animation::String,
    _atlas::String,
    _frame::Int,
    _animating::Bool,
    _tint::Color,
    _tintWeight::Float,
    _frameRate::Float,
    _ssz :: Int
  } deriving Show
makeLenses ''Sprite

newSprite :: String -> String -> PhysShape -> Sprite
newSprite atlas sprite shape = Sprite shape sprite atlas 0 False 1 0 60 0

spriteWire :: (MonadFix m, Monad m, HasTime t s) => Sprite -> Wire s e m (PhysShape,Event Bool,Event (Sprite -> Sprite)) Sprite
spriteWire sprite@Sprite{_frame=fram,_animating=animatin,_frameRate=fr} =
  let animate x = arr (\(_,fr,t) -> ( floor (t*fr)) )
      animator sp@Sprite{_frame=fram,_animating=animatin,_frameRate=fr} =
        dkSwitch (if animatin then animate fram else pure fram)
                (arr $ \((e,fr,t),f)-> fmap (const.animator) e )
      spriteReactive sp = dSwitch  ( (delay sp <<< arr fst) &&&  arr (\(s,e) -> spriteReactive <$> e ))
  in proc (shap,animationState,modif) -> do
      t <- timeF -< ()
      rec let spMod = fmap ($ sprite')$ merge (.) modif $ fmap (\state sprite -> sprite & animating .~ state ) animationState
          sprite' <- spriteReactive sprite -<
                (sprite' & shape .~ shap,spMod)
      f <- animator sprite -< (spMod,_frameRate sprite',t)
      returnA -< ( sprite' & frame .~ f)

instance Shaped Sprite where
  shape = sShape

instance Tinted Sprite where
  color = tint

instance ZIndexed Sprite where
  zIndex = ssz

instance Poseable Sprite where
  pos = shape.Geometry.position
  angle = shape.orient

instance Drawable Sprite where
  toDrawableObject sprite = DrawableObject {
    z = sprite^.ssz,
    resources  =
      [RO (get (sprite^.atlas) :: Getter Atlas)],
    setup = return,
    draw  = \r -> do
      atlas <- fst <$> get (sprite^.atlas) r ::  IO Atlas
      let (VB buffer (ih,VertexArrayDescriptor c d s p)) = atlas^.coords
          prog = atlas^.aMaterial.program
          (V4 r g b a ) = sprite^.tint
          anims = sprite^.animation :: String
          (Just animationObj) = atlas^.animations.at anims
          fram = sprite^.frame `rem` V.length (animationObj^.frames)
          indx = animationObj ^?! frames.ix fram
      setUniform prog "uTintWeight" $ sprite^.tintWeight
      setUniform prog "uTint" $ Vector4 r g b a
      setUniform prog "uMVMatrix" =<< toGlmatrix ((convertTransform $ sprite^.shape.trans :: M44 Double)& _z._w.~fromIntegral (sprite^.zIndex))
      maybe (return ()) (\attrib -> do
          bindBuffer ArrayBuffer $= Just buffer
          vertexAttribPointer attrib $= (ih,VertexArrayDescriptor c d s (plusPtr (nullPtr:: Ptr Word8) (4*2*4*indx)))
        )  (prog^.attributes.at "aTexCoord")
      drawArrays Quads 0 4
  }

sprite :: V2 Double -> V2 Double -> String -> String -> Sprite
sprite pos dim animation atlas =
  Sprite (OBB $ Transform pos dim 0)
          animation
          atlas
          0
          False
          (pure 0)
          0
          60
          0
