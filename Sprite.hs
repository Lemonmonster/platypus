{-#LANGUAGE TemplateHaskell#-}

module Sprite where

import Renderer
import Control.Lens hiding (Getter)
import Graphics.Rendering.OpenGL hiding (color,Color,get)
import Linear (V2(..),V4(..),M44,_z,_w)
import Geometry
import Atlas
import Control.Monad
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

type Color = V4 Float
data Sprite = Sprite{
    _sShape::PhysShape,
    _animation::String,
    _atlas::String,
    _frame::Int,
    _animating::Bool,
    _tint::Color,
    _tintWeight::Float,
    _ssz :: Int
  } deriving Show
makeLenses ''Sprite

instance Shaped Sprite where
  shape = sShape

instance Tinted Sprite where
  color = tint


instance Drawable Sprite where
  zIndex = ssz
  resources sprite =
    [RO (get (sprite^.atlas) :: Getter Atlas)]
  draw r sprite = do
    atlas <- fst <$> get (sprite^.atlas) r ::  IO Atlas
    let (VB buffer (ih,VertexArrayDescriptor c d s p)) = atlas^.coords
        prog = atlas^.aMaterial.program
        (V4 r g b a ) = sprite^.tint
        anims = sprite^.animation :: String
        (Just animationObj) = atlas^.animations.at anims
        indx = animationObj ^?! frames.ix ((sprite^.frame) `mod` (V.length (animationObj^.frames)))
    setUniform prog "uTintWeight" $ sprite^.tintWeight
    setUniform prog "uTint" $ Vector4 r g b a
    setUniform prog "uMVMatrix" =<< toGlmatrix ((convertTransform $ sprite^.shape.trans :: M44 Float)& _z._w.~fromIntegral (sprite^.zIndex))
    maybe (return ()) (\attrib -> do
        bindBuffer ArrayBuffer $= Just buffer
        vertexAttribPointer attrib $= (ih,VertexArrayDescriptor c d s (plusPtr (nullPtr:: Ptr Word8) (8*4*(sprite^.frame))))
      )  (prog^.attributes.at "aTexCoord")
    drawArrays Quads 0 4

sprite :: V2 Float -> V2 Float -> String -> String -> Sprite
sprite pos dim animation atlas =
  Sprite (OBB $ Transform pos dim 0)
          animation
          atlas
          0
          False
          (pure 0)
          0
          0
