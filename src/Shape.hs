{-#LANGUAGE TemplateHaskell#-}
module Shape where

import Renderer
import Geometry
import Linear.V4
import Linear.V2
import Control.Lens hiding (Getter)
import Material
import Graphics.Rendering.OpenGL as GL hiding (get,color,PolygonMode(..))
import Data.Map as Map
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Typeable
import Debug.Trace

data Shape = Shape{
   _sShape::PhysShape,
   _sColor::V4 Float,
   _sMaterial::String,
   _sz :: Int
} deriving (Show,Eq,Ord,Typeable)
makeLenses ''Shape

newShape:: PhysShape -> Shape
newShape shape = Shape shape (V4 1 1 1 1) "SimpleGeo" (-1)

geoBufferName = "dynamicGeometryBuffer"

instance Tinted Shape where
  color = sColor

instance Shaped Shape where
  shape = sShape

instance Materialed Shape where
  material = sMaterial

instance Drawable Shape where
  zIndex = sz
  setup r s =
    if Map.member geoBufferName (r^.globalArrays)  then
      return r
    else do
      buffer <- genObjectName :: IO BufferObject
      return $ r & globalArrays.at geoBufferName._Just.~ VB buffer (ToFloat,VertexArrayDescriptor 4 Float 0 nullPtr)
  resources s =
    [RO (get (s^.material) :: Getter Material)]
  draw r s = do
    (mat,_) <- get (s^.material) r
    let prog = mat^.program
        bindArray nm = do
          let (VB buffer desc) = r^?!globalArrays.at nm._Just
          maybe (return ()) (\attrib -> do
              bindBuffer ArrayBuffer $= Just buffer
              vertexAttribPointer attrib $= desc
            )  (prog^.attributes.at "aVertexPosition")
    setUniform prog "uTint" ((\(V4 r g b a) -> Vector4 r g b a) $ s^.color)
    mmat <- toGlmatrix ((convertTransform (s^.shape.trans) :: V4 (V4 Float)) & _z._w.~ fromIntegral (s^.zIndex))
    setUniform prog "uMVMatrix" mmat
    (case s^.shape of
      (Line t) -> do
        bindArray "line"
        drawArrays Lines 0 2
      (Circle t) -> do
        bindArray "circle"
        drawArrays GL.Polygon 0 $ floor circleVerts
      (OBB t) -> do
        bindArray "quad"
        drawArrays Quads 0 4
      (Box p d) -> do
        bindArray "quad"
        drawArrays Quads 0 4
      (Geometry.Polygon t ps) -> do
        let (VB buffer desc) = r^?!globalArrays.at geoBufferName._Just
            lst = concatMap (\(V2 x y) -> [x,y,0,1]) ps
        maybe (return ()) (\attrib -> do
            array <- newArray lst
            bindBuffer ArrayBuffer $= Just buffer
            bufferData ArrayBuffer $= (fromIntegral $ length lst * sizeOf (head lst),array,DynamicDraw)
            vertexAttribPointer attrib $= desc
            free array
            drawArrays GL.Polygon 0 (fromIntegral $ length ps)
          ) (prog^.attributes.at "aPosition")
      )


--instance Drawable Shape where
