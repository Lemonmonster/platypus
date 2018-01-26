{-#LANGUAGE TemplateHaskell#-}
module TextBlock where

import Linear.V4
import Linear.V2
import Renderer
import Geometry
import Control.Lens hiding (Getter,trace)
import Font
import Data.Maybe
import Data.Function
import Data.Bool
import Control.Monad
import Linear.Matrix hiding (trace)
import Graphics.Rendering.OpenGL hiding (get,scale,color,position)
import Data.Word
import Foreign.Ptr
import Debug.Trace

defaultFont :: String
defaultFont = "LiberationSerif-Regular:24"

data Alignment = ALeft | ARight | ACenter deriving Show

data TextBox = TextBox{
  _text :: String,
  _font :: String,
  _tColor :: V4 Float,
  _box :: PhysShape, -- draws the text in the box supplied here
  _textScale :: Float,
  _alignment :: Alignment,
  _tz :: Int
} deriving Show
makeLenses ''TextBox

instance Tinted TextBox where
  color = tColor

instance Shaped TextBox where
  shape = box

textBox:: String -> String -> PhysShape -> TextBox
textBox text font shape =
  TextBox text font (V4 0 0 0 1) shape 1 ACenter 0

instance Drawable TextBox where
  zIndex = tz
  resources t =
    [RO (get $ t^.font :: Getter Font)]
  draw r t = do
    (fnt,_) <- get (t^.font) r
    let (V2 w h) = t^.box.trans.scale
        (V2 bx by) = t^.box.position
        advanc c = fromIntegral (fnt^?!glyphs.at c._Just.advance) * t^.textScale
        getLn wLeft [] wrd line =
          bool Nothing (Just (w-wLeft,(line . wrd) [], [])) ( wLeft >=0 || (null $ line []))
        getLn wLeft (' ':xs) wrd line =
          if wLeft >=0 || (null $ line []) then
            let a = getLn (wLeft - advanc ' ') xs id (line . wrd . (' ':))
            in  Just $ fromMaybe (w-wLeft,(line . wrd) [],xs) a
          else
            Nothing
        getLn wLeft ('\n':xs) wrd line =
          if wLeft >=0 || (null $ line []) then
            if null (line []) && null (wrd []) then
              Just (0,[],xs)
            else
              Just (w-wLeft,(line . wrd) [],'\n':xs)
          else Nothing
        getLn wLeft (a:xs) wrd line =
          getLn (wLeft - advanc a) xs (wrd . (a:)) line
        convert:: Float -> Float
        convert l =
          case t^.alignment of
            ALeft -> 0
            ARight -> w - l
            ACenter -> (w - l)/2
        lins :: [(Float,String)]
        lins = fix (\f string ->
                let (Just (l,s,string')) = getLn w string id id
                in if null string' then
                      [(convert l,s)]
                   else
                    (convert l,s):f string'
                ) $ t^.text
    let mt = convertTransform ((t^.box.trans) & scale .~ pure 1) & _z._w.~ fromIntegral (t^.zIndex) :: V4 (V4 Float)
    foldM_ (\h' (x,str) -> do
        foldM_ (\x' c -> do
          let (Glyph gw advanc gh indx) = fnt^?!glyphs.at c._Just
              box = Box (V2 x' h') (fromIntegral <$> V2 gw gh)
              prog = fnt^.fmaterial.program
              (VB buffer (ih,VertexArrayDescriptor cv d s p)) = fnt^.glyphCoords
          mmatrix <- toGlmatrix (mt !*! convertTransform (box^.trans))
          setUniform prog "uMVMatrix" mmatrix
          setUniform prog "uTint" $ (\(V4 r g b a)-> Vector4 r g b a) $ t^.color
          setUniform prog "uTintWeight" (1 :: GLfloat)
          maybe (return ()) (\attrib -> do
              bindBuffer ArrayBuffer $= Just buffer
              vertexAttribPointer attrib $= (ih,VertexArrayDescriptor cv d s (plusPtr (nullPtr:: Ptr Word8) (8*4*indx)))
            )  (prog^.attributes.at "aTexCoord")
          drawArrays Quads 0 4
          return $ x' + fromIntegral advanc
         ) (x-(w/2)) str
        return ( h' - fromIntegral (fnt^.lineSkip)) :: IO Float
      ) (h/2 - fromIntegral (fnt^.lineSkip)) lins
