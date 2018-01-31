{-#LANGUAGE ExistentialQuantification#-}
{-#LANGUAGE DataKinds#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE MultiWayIf#-}
{-#LANGUAGE TupleSections#-}
module Renderer where

import Graphics.Rendering.OpenGL hiding (Color,Texture,get,viewport)
import qualified Graphics.Rendering.OpenGL as GL
import Codec.Picture
import qualified Data.Map.Strict as Map
import Geometry
import Linear (V2(..),V4(..))
import Text.Regex.PCRE
import Control.Monad
import qualified Data.Vector as VG
import qualified Data.Vector.Storable as V
import Data.Function
import Control.Lens hiding (Getter)
import System.Directory
import Data.Bool
import Data.List.Split
import Data.List
import Data.Semigroup
import Data.Dynamic
import Data.Maybe
import Data.Map.Lens
import Data.Either (isLeft,isRight)
import Text.Read (readMaybe)
import Control.Applicative
import Data.Ord
import Linear.V4
import Linear.Matrix hiding (trace)
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import Debug.Trace

shaderPath = "res/shaders/"
materialPath = "res/materials/"
texturePath = "res/textures/"
fontPath = "res/fonts/"

projectionMatrix = "uPVMatrix"
maxZ = 100

circleVerts = 64;

geometryBuffers :: [(String,[GLfloat])]
geometryBuffers =
  [("quad",[-0.5,-0.5,0,1,0.5,-0.5,0,1,0.5,0.5,0,1,-0.5,0.5,0,1]),
   ("line",[0,0,0,1,1,1,0,1]),
   ("circle",concatMap ((\(V4 x y z w)-> [x,y,z,w]).
    (\i-> (convertTransform (Transform (pure 0) (pure 1) (2*pi*i/circleVerts)):: M44 Float )!* (V4 0.5 0 0 1) ))
      [0..circleVerts-1])
  ]
--classes for common graphics elements
class Named a where
  name :: Lens' a String

class Shaped a where
  shape :: Lens' a PhysShape

class Tinted a where
  color :: Lens' a (V4 Float)

class Materialed a where
  material :: Lens' a String

data Texture = Texture{_tName::String,_obj::TextureObject} deriving (Eq,Ord,Show)
makeLenses ''Texture

data ShaderProgram = Program{
    _sName :: String,
    _handle::Program,
    _attributes::(Map.Map String AttribLocation),
    _uniforms::(Map.Map String UniformLocation)
  } deriving (Eq, Show,Ord)
makeLenses ''ShaderProgram

data VertexBuffer = VB BufferObject (IntegerHandling,VertexArrayDescriptor ()) deriving (Eq,Ord,Show)

data UniformData = forall a . (Uniform a) => UD a
instance Show UniformData where
  show = const "UniformData"

--Either is used to store strings that refer to dynamic values in the scene
data Material = Material{
    _mName :: String,
    _textureVals :: [(String,Texture)],
    _uniformVals::[(String,Either String UniformData)],
    _attributeVals::[(String,Either String VertexBuffer)],
    _program::ShaderProgram
  }
makeLenses ''Material

instance Eq Material where
  (==) a b = _program a == _program b

instance Ord Material where
  compare a b = compare (_program a)  (_program b)

instance Show Material where
  show (Material s t a b p) = s ++ show (map fst t) ++ show (map fst a) ++ show (map fst b) ++ show p


data Animation = Animation{
    _rate :: Float,
    _frames :: VG.Vector Int
  } deriving (Eq,Ord, Show)
makeLenses ''Animation

data Atlas = Atlas {
  _aName::String,
  _aMaterial::Material,
  _animations::Map.Map String Animation,
  _coords :: VertexBuffer
} deriving (Eq,Ord,Show)
makeLenses ''Atlas

instance Named Atlas where
  name = aName


data Glyph = Glyph{
  _width::Int,
  _advance :: Int,
  _height :: Int,
  _index :: Int
} deriving (Show,Eq,Ord)
makeLenses ''Glyph

data Font = Font {
  _fName::String,
  _fmaterial::Material,
  _glyphs::Map.Map Char Glyph,
  _glyphCoords:: VertexBuffer,
  _rangeStart :: Int,
  _lineSkip :: Int
} deriving (Show,Eq,Ord)
makeLenses ''Font
instance Named Font where
  name = fName

data Renderer = Renderer{
  _viewport :: PhysShape,
  _delta :: Float,
  _projection :: Maybe (GLmatrix Float),
  _textures:: Map.Map String Texture,
  _shaders :: Map.Map String ShaderProgram,
  _materials :: Map.Map String Material,
  _atlases :: Map.Map String Atlas,
  _fonts :: Map.Map String Font,
  _globalArrays :: Map.Map String VertexBuffer,
  _globalUniforms :: Map.Map String UniformData,
  _other :: Map.Map String Dynamic
} deriving (Show)
makeLenses ''Renderer


{-
data Shape = Shape PhysShape Color
data Path =  Path [V2 Float] Color-}


type Getter a = (Renderer -> IO (a,Renderer))
data ResourceObject = forall a. (Resource a) => RO (Getter a)

class Resource a where
  identifier :: a -> (String,String) -- type and id
  get  :: String -> Renderer -> IO (a,Renderer)
  bind :: Renderer -> a -> IO ()
  bind = const $ const $ return ()
  unbind :: Renderer -> a -> IO ()
  unbind = const $ const $ return ()
  subResources:: a->[ResourceObject]
  subResources = const []
  unload :: a -> IO ()
  unload = const $ return ()

--minimal complete implementations does nothing
class Drawable a where
  setup :: Renderer -> a -> IO Renderer -- for seting up global uniforms and attributeNames
  setup r a= return r
  resources :: a -> [ResourceObject]
  resources = const []
  draw :: Renderer -> a -> IO ()
  draw = const $ const $ return ()
  zIndex :: Lens' a Int

toGlmatrix :: (Foldable t) => t (t Float) -> IO (GLmatrix Float)
toGlmatrix = (newMatrix RowMajor) . concat . (foldr ((:).(foldr (:) [])) [])

data DrawableObject = forall a. (Drawable a,Show a) => DO a
instance Show DrawableObject where
  show (DO a) = "(DO "++ show a ++ ")"

orthoM44 left right top bottom near far =
  V4 (V4 (2/(right-left)) 0 0 ( - (right+left)/(right-left)))
     (V4 0 (2/(top-bottom)) 0 ( - (top+bottom)/(top-bottom)))
     (V4 0 0 ((-2)/(far-near)) ( - (near+far)/(far-near)))
     (V4 0 0 0  1)

identityM44 =
  V4 (V4 1 0 0 0)
     (V4 0 1 0 0)
     (V4 0 0 1 0)
     (V4 0 0 0 1)

render :: Renderer -> Float-> [DrawableObject] -> IO Renderer
render r dt drawables = do
  cullFace $= Nothing
  depthFunc $= Just Lequal
  blend $= Enabled
  blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
  r' <-(\ r -> return $ r & delta .~ dt) =<<(foldM (\r (DO o)->setup r o) r drawables)
  let (Transform centr dim o) = r'^.viewport.trans
      mat = convertTransform (Transform (-centr) (2/dim) (-o)) & _z._z.~ (-1/maxZ) :: V4 (V4 Float)
  proj <- toGlmatrix $ mat
  let r'' = r' & projection .~ Just proj
  -- gets all the resources doing loading if necessary and puts the identifier into a list
  (r''',drawablesWithRids) <- foldM (\(rout,lst) (d,ros)-> do
            (rout',ids) <- foldM (\(rin,idlst) (RO getter) -> do
                (obj,rin') <- getter rin
                return (rin',identifier obj : idlst)
              ) (rout,[]) ros
            return (rout',(d,nub ids):lst)
          ) (r'',[]) $ map (\d@(DO a)-> (d, resources a)) drawables
  e <- errors
  unless (null e) (error ("opengl get error "++show e))
  let groupedDrawables =
        map (map fst) $ concatMap (groupBy ((flip $ (==).snd).snd) . sortBy (comparing snd))
          (groupBy (\(DO a,_) (DO b,_)-> a^.zIndex == b^.zIndex) $ sortBy (comparing (\(DO a,_)->a^.zIndex)) drawablesWithRids)
  rfinal <-foldM (\r drawableSet@((DO a):_)-> do
    r' <- foldM (\r (RO getter) -> do
        (obj,r'') <- getter r
        bind r'' obj
        return r''
      ) r (resources a)
    e <- errors
    unless (null e) (error ("opengl bind error "++show e))
    mapM_ (\(DO o) ->draw r o) drawableSet
    e <- errors
    unless (null e) (error ("opengl draw error "++show e))
    return r'
   ) r''' groupedDrawables
  finish
  return rfinal

listToBuffer:: (Storable a) => Int->BufferUsage -> IntegerHandling -> DataType -> [a] -> IO VertexBuffer
listToBuffer components usage ih typ lst = do
  let size = sizeOf $ head lst
  verts <- newArray lst
  buffer <- genObjectName :: IO BufferObject
  bindBuffer ArrayBuffer $= Just buffer
  bufferData ArrayBuffer $= (fromIntegral $ size *length lst, verts, usage)
  bindBuffer ArrayBuffer $= Nothing
  flush
  free verts
  return (VB buffer (ih,VertexArrayDescriptor (fromIntegral components) typ 0 nullPtr))

newRenderer w h = do
  buffers <- mapM (\(n,lst)-> fmap (n,) $ listToBuffer 4 StaticDraw ToFloat Float lst) geometryBuffers
  return $ Renderer
             (OBB $ Transform (pure 0) (V2 w h) 0)
             0
             Nothing
             Map.empty
             Map.empty
             Map.empty
             Map.empty
             Map.empty
             (Map.fromList buffers)
             Map.empty
             Map.empty

setUniform :: (Uniform a) => ShaderProgram -> String -> a -> IO ()
setUniform prog str val = maybe (return ()) (\u-> uniform u $= val) (prog^.uniforms.at str)
