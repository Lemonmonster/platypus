{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
module Material where

import Renderer
import Graphics.Rendering.OpenGL hiding (Color,Texture,get,viewport)
import qualified Graphics.Rendering.OpenGL as GL
import Codec.Picture
import qualified Data.Map.Strict as Map
import Geometry
import Linear (V2,V4)
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
import Debug.Trace



nameAndPath :: String -> String -> (String,String)
nameAndPath str path =
  if (str =~".*/.+") then
    (str,str =~"\\\\\\w*$")
  else
    (path++str,str)

instance Resource ShaderProgram where
  identifier p = ("shaderProgram",p^.sName)
  get str r =
    let (path,name) = nameAndPath str shaderPath
        val = r ^. shaders . at name
    in if isJust val then
          return (fromJust val,r)
       else do
        frags <- readFile (path ++ ".frag")
        verts <- readFile (path ++ ".vert")
        let uniformPattern = "uniform\\s+\\w+\\s+(\\w+);"
            attributePattern = "attribute\\s+\\w+\\s+(\\w+);"
            flatten =  map (head.tail)
            uniformNames = flatten (frags =~ uniformPattern :: [[String]]) ++
                           flatten (verts =~ uniformPattern :: [[String]])
            attributeNames = flatten (verts =~ attributePattern :: [[String]])
        p <- createProgram
        fs <- createShader FragmentShader
        vs <- createShader VertexShader
        print "attributes"
        print attributeNames
        shaderSourceBS fs $= packUtf8 frags
        compileShader fs
        shaderInfoLog fs >>= (\s -> case s of
          _  -> do
            shaderSourceBS vs $= packUtf8 verts
            compileShader vs
            shaderInfoLog vs >>= (\s -> case s of
              _ -> do
                attachShader p fs
                attachShader p vs
                linkProgram p
                linkStatus p >>= (\link ->
                  if link then do
                     umap <- return . Map.fromList =<< mapM (\s-> GL.get (uniformLocation p s) >>= (\v -> return (s,v)))  uniformNames
                     amap <- return . Map.fromList =<< mapM (\s-> GL.get (attribLocation p s) >>= (\v -> return (s,v)))  attributeNames
                     errors >>= (\es -> case es of
                       _ ->
                        let progrm = Program name p amap umap
                        in return (progrm,r & shaders . at name .~ Just progrm)
                       _  -> error (show es)
                      )
                  else
                    error ("shader program link error "++ name)
                 )
              _  -> error ("vertex shader :" ++ name ++ "\n" ++ s)
             )
          _ -> error ("fragment shader : " ++ name ++ "\n" ++ s)
         )

  bind r (Program _ handle attribs uniforms) = do
    currentProgram $= Just handle
    mapM_ (\val ->vertexAttribArray val $= Enabled) attribs

imageToTexture :: (Pixel a) => Image a -> IO TextureObject
imageToTexture (Image w h v) =
  V.unsafeWith v (\ ptr -> do
    let nearestPower = (fix (\f i x -> if x<=i then i else f (i*2) x)) 1
    let  size = TextureSize2D (fromIntegral  w) (fromIntegral  h)
    tex <- genObjectName :: IO TextureObject
    textureBinding Texture2D $= Just tex
    textureFilter Texture2D $= ( (Linear', Just Linear'), Linear')
    textureWrapMode Texture2D S $= (Repeated,Repeat)
    textureWrapMode Texture2D T $= (Repeated,Repeat)
    texImage2D Texture2D NoProxy 0 RGBA' size 0 (PixelData RGBA UnsignedByte ptr)
    generateMipmap' Texture2D
    flush
    textureBinding Texture2D $= Nothing
    return tex
   )


--TODO guarantee texture as dimensions that are powers of two
instance Resource Texture where
  identifier p = ("texture",p^.tName)
  get file r =
    let (path,name) = nameAndPath file texturePath
    in case (r^.textures.at name) of
        (Just txt) -> return (txt,r )
        Nothing    ->do
          image <- readImage file
          (case image of
            (Right dimage) -> do
                    let image' = convertRGBA8 dimage
                    t <- return . Renderer.Texture name =<< imageToTexture image'
                    return (t,r & textures . at name .~ Just t)
            (Left s) -> error ("error loading texture "++file++": "++s)
            )

instance Resource Material where
  identifier a = ("material",a^.mName)
  get file r =
    let (path,name) = nameAndPath file materialPath
        m = r^.materials.at name
    in if isJust m then
        return (fromJust m,r)
       else do
        print (path,name)
        statements <-(return. map words . lines ) =<< readFile (path++".mat")
        let maybeShader = find (("shader" ==). head ) statements
        case maybeShader of
          (Just (shaderName:xs)) -> do
            let val = foldl1' (\a b-> a++" "++b) xs
            (shader,r) <- Renderer.get val r
            let merge = foldl' (\a b-> a++" "++b) ""
            (mat,r')<-foldM (\(material,r') wrds ->
              case wrds of
                ("uniform":name:xs) -> do
                  let val = merge xs
                      (Just value) =
                            (Right . UD <$> (readMaybe val :: Maybe GLint)) <|>
                            (Right . UD <$> (readMaybe val :: Maybe GLuint)) <|>
                            (Right . UD <$> (readMaybe val :: Maybe GLfloat)) <|>
                            (Right . UD <$> (readMaybe val :: Maybe GLdouble)) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Index1 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Index1 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Index1 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Index1 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Color4 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Color4 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Color4 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Color4 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Color3 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Color3 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Color3 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Color3 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (FogCoord1 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (FogCoord1 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (FogCoord1 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (FogCoord1 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Normal3 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Normal3 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Normal3 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Normal3 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord4 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord4 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord4 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord4 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord3 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord3 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord3 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord3 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord2 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord2 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord2 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord2 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord1 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord1 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord1 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (TexCoord1 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector4 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector4 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector4 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector4 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector3 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector3 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector3 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector3 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector2 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector2 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector2 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector2 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector1 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector1 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector1 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vector1 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex4 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex4 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex4 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex4 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex3 GLint)))  <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex3 GLuint)))  <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex3 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex3 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex2 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex2 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex2 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex2 GLdouble))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex1 GLint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex1 GLuint))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex1 GLfloat))) <|>
                            (Right . UD <$> (readMaybe val :: Maybe (Vertex1 GLdouble))) <|>
                            (Just (Left val))
                  when (isLeft value) (print ("Material:"++path++" cannot read uniform value "++val))
                  return (material & uniformVals %~ ((name,value):),r' )
                ("attribute":name:ident:xs) ->
                  return (material & attributeVals %~ ((name,Left $ ident):),r')
                ("texture":name:xs) -> do
                  let val = merge xs
                  (tex,r) <- Renderer.get val r'
                  return (material & textureVals %~ ((name,tex):),r)
                _ -> do
                  print $ "blank line in material "++path++" with words:"
                  print wrds
                  return (material,r')
              ) (Material name [] [] [] shader,r) statements
            return (mat,r' & materials.at name.~ Just mat)
          Nothing -> error $ "Material "++path++" has no shader specified"
  bind r m = do
    let s = m^.program
    bind r s
    e <- errors
    unless (null e) (error ("opengl shader bind error "++show e))
    mapM_ (\(name,val)->do
      let unif = s^.uniforms.at name
      case val of
        (Right (UD uniformVal)) -> do
          when (isJust unif) $ (uniform (fromJust unif) $= uniformVal)
          --print name
        (Left str) -> do
          let globalUnif = r^.globalUniforms.at str
          when (isJust unif && isJust globalUnif) $ (\(UD a)-> uniform (fromJust unif) $= a) (fromJust globalUnif)
          --print name
      ) $ m^.uniformVals
    e <- errors
    unless (null e) (error ("opengl uniform bind error "++show e))
    foldM_ (\i (name,tex) ->
      case s^.uniforms.at name of
        (Just sampler)-> do
          --print name
          textureBinding Texture2D $= Nothing
          activeTexture $= TextureUnit (fromIntegral i)
          textureBinding Texture2D $= (Just (tex^.obj))
          uniform sampler $= (fromIntegral 0 :: GLint)
          return $ i+1
        Nothing -> do
          print ("fail"++name)
          return i
        ) 0 $ m^.textureVals
    e <- errors
    unless (null e) (error ("opengl texture bind error "++show e))
    mapM_ (\(name,val)->
      let attrib = s^.attributes.at name
      in case val of
          (Right (VB buffer desc)) -> do
            when (isJust attrib) $ do
              bindBuffer ArrayBuffer $= Just buffer
              vertexAttribPointer (fromJust attrib) $= desc
              --print name
          (Left str) -> do
            let globalAttrib = r^.globalArrays.at str
            when (isJust attrib && isJust globalAttrib) $ do
              let (VB buffer desc) = fromJust globalAttrib
              bindBuffer ArrayBuffer $= Just buffer
              vertexAttribPointer (fromJust attrib) $= desc
      ) $ m^.attributeVals
    e <- errors
    unless (null e) (error ("opengl array bind error "++show e))
    case m^.program.uniforms.at projectionMatrix of
      (Just u) -> uniform u $= r ^?! projection._Just
      _ -> return ()
  subResources m =
     RO (get (m^.program.sName) :: Getter ShaderProgram) :
     map ((\t-> RO (get (t^.tName) :: Getter Texture)).snd) (m^.textureVals)
