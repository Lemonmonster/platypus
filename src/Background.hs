{-#LANGUAGE TemplateHaskell#-}
module Background where

import Prelude hiding ((.),id)
import Renderer
import Linear
import Graphics.Rendering.OpenGL as GL hiding (color,Color,get)
import Foreign.Ptr
import Data.Word
import Data.Ord
import Geometry
import Atlas
import Material
import Control.Lens
import Data.List
import Data.Fixed
import Wires hiding (at)
import Debug.Trace
import qualified Data.Vector as V

epsilon = 4

data Cell = Cell{
    _animation :: String,
    _framerate :: Double,
    _tint :: V4 Float,
    _tintWeight :: Float,
    _frame :: Int
} deriving (Eq,Ord,Show)
makeLenses ''Cell
data Layer = Layer{
    _cells :: [Cell],
    _size::V2 Double,
    _yRange :: (Double,Double),
    _backZ :: Double -- used to calculate parallax (-2 = x/2, 2 = x*3)
} deriving (Eq,Ord,Show)
makeLenses ''Layer
data Background = Background{
  _atlas::String,
  _layers::[Layer],
  _bounds::PhysShape
} deriving (Eq,Ord,Show)
makeLenses ''Background

instance Drawable Background where
  toDrawableObject (Background bAtlas layers shape) =  DrawableObject {
    z = -99,
    resources =  [RO (get (bAtlas) :: Renderer.Getter Atlas)],
    setup = return,
    draw = \r -> do
      atlas <- fst <$> get (bAtlas) r ::  IO Atlas
      let (VB buffer (ih,VertexArrayDescriptor c d s p)) = atlas^.coords
          prog = atlas^.aMaterial.program
          (Box pos@(V2 xmin ymin) dim) = toBound shape
          (V2 xmax ymax) = pos + dim
          (V2 cx cy) = pos + dim/2
      mapM_ (\ (Layer cells (V2 w h) (yrmin,yrmax) _backZ) -> do
          let parallax = if _backZ >=0
                            then _backZ + 1
                            else 1/abs _backZ
              xoffset = (parallax*cx) `mod'` w
              yoffset = (parallax*cy) 
              yrmin' =  yrmin + cy - yoffset 
              yrmax' = yrmax + cy - yoffset
              xi = floor (xmin/w)
              yi = max 0 $ floor ((ymin-yrmin')/h)
              xstart = ( w* fromIntegral xi ) + ( xoffset - w )
              ystart =  yrmin' + (h* fromIntegral yi)
          --polygonMode $= (GL.Line,GL.Line)
          mapM_ (\ (Cell animation _ (V4 r g b a) tintWeight frame ,(x,y)) -> do
                let transf = Box (V2 x y) (V2 w h) ^. trans
                    (Just animationObj) = atlas^.animations.at animation
                    fram = frame `rem` V.length (animationObj^.frames)
                    indx = animationObj ^?! frames.ix fram
                setUniform prog "uTintWeight" tintWeight
                setUniform prog "uTint" $ Vector4 r g b a
                setUniform prog "uMVMatrix" =<< toGlmatrix ((convertTransform $ transf :: M44 Double)& _z._w.~(-99.99))
                maybe (return ()) (\attrib -> do
                   bindBuffer ArrayBuffer $= Just buffer
                   vertexAttribPointer attrib $= (ih,VertexArrayDescriptor c d s (plusPtr (nullPtr:: Ptr Word8) (4*2*4*indx)))
                 )  (prog^.attributes.at "aTexCoord")
                drawArrays Quads 0 4
           ) $ zip
                (cycle cells)
                $  [(x,y) | y <- takeWhile (liftA2 (&&) ( yrmax' - epsilon > ) (ymax>)) [ystart,ystart+h..],
                            x <- takeWhile (xmax>) [xstart,xstart+w..]]
       ) $  sortBy (comparing _backZ) layers 


}

--convenience function 
backgroundW:: (Monad m) => Background -> Wire s e m PhysShape DrawableObject
backgroundW background = arr (tdo.($ background).(bounds .~ )) 
