{-#LANGUAGE TupleSections#-}
module Font where

import Atlas (packImages)
import Material
import Renderer
import SDL.Font
import qualified Data.Map.Strict as Map
import Control.Lens hiding (Getter,index)
import Data.Maybe
import Data.List.Split
import Text.Read hiding (get)
import Control.Monad
import Data.List
import Data.Monoid
import Control.Applicative
import Data.Map.Lens
import Data.Char
import SDL.Video.Renderer
import Foreign.Storable
import qualified SDL.Raw.Types as Raw
import Linear.V4
import Linear.V2
import Graphics.Rendering.OpenGL hiding (get,index)
import SDL.Video.Renderer
import SDL.Vect
import SDL.Raw.Video (saveBMP)
import qualified SDL.Raw.Types (Surface(..))
import Foreign.C.String
import Debug.Trace
import Codec.Picture

unicodeRanges :: Map.Map String (Int,Int)
unicodeRanges =
  Map.fromList $ map (\(a,b)->(b,a))
    [((0x0000,0x007F),  "Basic Latin"),
    ((0x0080,	0x00FF),  "C1 Controls and Latin-1 Supplement"),
    ((0x0100,	0x017F),  "Latin Extended-A"),
    ((0x0180,	0x024F),  "Latin Extended-B"),
    ((0x0250,	0x02AF),  "IPA Extensions"),
    ((0x02B0,	0x02FF),  "Spacing Modifier Letters"),
    ((0x0300,	0x036F),  "Combining Diacritical Marks"),
    ((0x0370,	0x03FF),  "Greek/Coptic"),
    ((0x0400,	0x04FF),  "Cyrillic"),
    ((0x0500,	0x052F),  "Cyrillic Supplement"),
    ((0x0530,	0x058F),  "Armenian"),
    ((0x0590,	0x05FF),  "Hebrew"),
    ((0x0600,	0x06FF),  "Arabic"),
    ((0x0700,	0x074F),  "Syriac"),
    ((0x0750,	0x077F),  "Undefined"),
    ((0x0780,	0x07BF),  "Thaana"),
    ((0x07C0,	0x08FF),  "Undefined"),
    ((0x0900,	0x097F),  "Devanagari"),
    ((0x0980,	0x09FF),  "Bengali/Assamese"),
    ((0x0A00,	0x0A7F),  "Gurmukhi"),
    ((0x0A80,	0x0AFF),  "Gujarati"),
    ((0x0B00,	0x0B7F),  "Oriya"),
    ((0x0B80,	0x0BFF),  "Tamil"),
    ((0x0C00,	0x0C7F),  "Telugu"),
    ((0x0C80,	0x0CFF),  "Kannada"),
    ((0x0D00,	0x0DFF),  "Malayalam"),
    ((0x0D80,	0x0DFF),  "Sinhala"),
    ((0x0E00,	0x0E7F),  "Thai"),
    ((0x0E80,	0x0EFF),  "Lao"),
    ((0x0F00,	0x0FFF),  "Tibetan"),
    ((0x1000,	0x109F),  "Myanmar"),
    ((0x10A0,	0x10FF),  "Georgian"),
    ((0x1100,	0x11FF),  "Hangul Jamo"),
    ((0x1200,	0x137F),  "Ethiopic"),
    ((0x1380,	0x139F),  "Undefined"),
    ((0x13A0,	0x13FF),  "Cherokee"),
    ((0x1400,	0x167F),  "Unified Canadian Aboriginal Syllabics"),
    ((0x1680,	0x169F),  "Ogham"),
    ((0x16A0,	0x16FF),  "Runic"),
    ((0x1700,	0x171F),  "Tagalog"),
    ((0x1720,	0x173F),  "Hanunoo"),
    ((0x1740,	0x175F),  "Buhid"),
    ((0x1760,	0x177F),  "Tagbanwa"),
    ((0x1780,	0x17FF),  "Khmer"),
    ((0x1800,	0x18AF),  "Mongolian"),
    ((0x18B0,	0x18FF),  "Undefined"),
    ((0x1900,	0x194F),  "Limbu"),
    ((0x1950,	0x197F),  "Tai Le"),
    ((0x1980,	0x19DF),  "Undefined"),
    ((0x19E0,	0x19FF),  "Khmer Symbols"),
    ((0x1A00,	0x1CFF),  "Undefined"),
    ((0x1D00,	0x1D7F),  "Phonetic Extensions"),
    ((0x1D80,	0x1DFF),  "Undefined"),
    ((0x1E00,	0x1EFF),  "Latin Extended Additional"),
    ((0x1F00,	0x1FFF),  "Greek Extended"),
    ((0x2000,	0x206F),  "General Punctuation"),
    ((0x2070,	0x209F),  "Superscripts and Subscripts"),
    ((0x20A0,	0x20CF),  "Currency Symbols"),
    ((0x20D0,	0x20FF),  "Combining Diacritical Marks for Symbols"),
    ((0x2100,	0x214F),  "Letterlike Symbols"),
    ((0x2150,	0x218F),  "Number Forms"),
    ((0x2190,	0x21FF),  "Arrows"),
    ((0x2200,	0x22FF),  "Mathematical Operators"),
    ((0x2300,	0x23FF),  "Miscellaneous Technical"),
    ((0x2400,	0x243F),  "Control Pictures"),
    ((0x2440,	0x245F),  "Optical Character Recognition"),
    ((0x2460,	0x24FF),  "Enclosed Alphanumerics"),
    ((0x2500,	0x257F),  "Box Drawing"),
    ((0x2580,	0x259F),  "Block Elements"),
    ((0x25A0,	0x25FF),  "Geometric Shapes"),
    ((0x2600,	0x26FF),  "Miscellaneous Symbols"),
    ((0x2700,	0x27BF),  "Dingbats"),
    ((0x27C0,	0x27EF),  "Miscellaneous Mathematical Symbols-A"),
    ((0x27F0,	0x27FF),  "Supplemental Arrows-A"),
    ((0x2800,	0x28FF),  "Braille Patterns"),
    ((0x2900,	0x297F),  "Supplemental Arrows-B"),
    ((0x2980,	0x29FF),  "Miscellaneous Mathematical Symbols-B"),
    ((0x2A00,	0x2AFF),  "Supplemental Mathematical Operators"),
    ((0x2B00,	0x2BFF),  "Miscellaneous Symbols and Arrows"),
    ((0x2C00,	0x2E7F),  "Undefined"),
    ((0x2E80,	0x2EFF),  "CJK Radicals Supplement"),
    ((0x2F00,	0x2FDF),  "Kangxi Radicals"),
    ((0x2FE0,	0x2FEF),  "Undefined"),
    ((0x2FF0,	0x2FFF),  "Ideographic Description Characters"),
    ((0x3000,	0x303F),  "CJK Symbols and Punctuation"),
    ((0x3040,	0x309F),  "Hiragana"),
    ((0x30A0,	0x30FF),  "Katakana"),
    ((0x3100,	0x312F),  "Bopomofo"),
    ((0x3130,	0x318F),  "Hangul Compatibility Jamo"),
    ((0x3190,	0x319F),  "Kanbun (Kunten)"),
    ((0x31A0,	0x31BF),  "Bopomofo Extended"),
    ((0x31C0,	0x31EF),  "Undefined"),
    ((0x31F0,	0x31FF),  "Katakana Phonetic Extensions"),
    ((0x3200,	0x32FF),  "Enclosed CJK Letters and Months"),
    ((0x3300,	0x33FF),  "CJK Compatibility"),
    ((0x3400,	0x4DBF),  "CJK Unified Ideographs Extension A"),
    ((0x4DC0,	0x4DFF),  "Yijing Hexagram Symbols"),
    ((0x4E00,	0x9FAF),  "CJK Unified Ideographs"),
    ((0x9FB0,	0x9FFF),  "Undefined"),
    ((0xA000,	0xA48F),  "Yi Syllables"),
    ((0xA490,	0xA4CF),  "Yi Radicals"),
    ((0xA4D0,	0xABFF),  "Undefined"),
    ((0xAC00,	0xD7AF),  "Hangul Syllables"),
    ((0xD7B0,	0xD7FF),  "Undefined"),
    ((0xD800,	0xDBFF),  "High Surrogate Area"),
    ((0xDC00,	0xDFFF),  "Low Surrogate Area"),
    ((0xE000,	0xF8FF),  "Private Use Area"),
    ((0xF900,	0xFAFF),  "CJK Compatibility Ideographs"),
    ((0xFB00,	0xFB4F),  "Alphabetic Presentation Forms"),
    ((0xFB50,	0xFDFF),  "Arabic Presentation Forms-A"),
    ((0xFE00,	0xFE0F),  "Variation Selectors"),
    ((0xFE10,	0xFE1F),  "Undefined"),
    ((0xFE20,	0xFE2F),  "Combining Half Marks"),
    ((0xFE30,	0xFE4F),  "CJK Compatibility Forms"),
    ((0xFE50,	0xFE6F),  "Small Form Variants"),
    ((0xFE70,	0xFEFF),  "Arabic Presentation Forms-B"),
    ((0xFF00,	0xFFEF),  "Halfwidth and Fullwidth Forms"),
    ((0xFFF0,	0xFFFF),  "Specials")]

type CharSurf = (Char,(Glyph,Surface))

instance Resource Renderer.Font where
  identifier f = ("font",f^.fName)
  get file r =
    let fname:dat = splitOn ":" file
        siz = fromMaybe "12" $ dat ^? ix 0
        language = fromMaybe "Basic Latin" $ dat ^? ix 1
        nm = (fname++":"++siz++":"++language)
        (path,name) = nameAndPath fname fontPath
        font = r ^. fonts . at nm
    in if isJust font then do
          return (fromJust font,r)
       else do
        let sz = readMaybe siz
        let ranges = foldl' (\a b -> (:) <$> b <*> a) (Just []) (map (\s -> unicodeRanges ^. at s) (splitOn "-" language))
        unless (isJust sz) (error "Invalid font size")
        unless (isJust ranges) (error "Invalid font size")
        initialize
        font <- load (path++".ttf") (fromJust sz)
        let range = foldl1' (\(a,b) (c,d) -> (min a b, max c d)) (fromJust ranges)
        chars <- filterM (glyphProvided font) $ map toEnum [fst range..snd range]
        let renderChar c = do
             metric <- glyphMetrics font c
             case metric of
               (Just (minx',maxx',miny',maxy',advance')) -> do
                 --print metric
                 surf <- blendedGlyph font (V4 0 0 0 255) c
                 --print =<< surfaceDimensions surf
                 (V2 w h)<- surfaceDimensions surf
                 return $ Just ((c,(Glyph (fromIntegral w) advance' (fromIntegral h) 0 ,surf)),(w,h))
               _ ->
                 return Nothing
        charSurfaces <- fmap catMaybes $ mapM renderChar chars
        let area = foldl' (\v (_,(w,h))-> v + (w*h)) 0 charSurfaces
            side = 2 ^ (ceiling (log (fromIntegral area)/log 4))
            (charSurface',(side',_)) = packImages (\(x,y)->(x*2,y*2)) (side,side) charSurfaces
        surface <- createRGBSurface (V2 side' side') ARGB8888
        surfaceFillRect surface Nothing (V4 0 0 0 0)
        mapM_ (\((_,(_,s)),(x,y,w,h))->
            surfaceBlit s Nothing surface (Just $ P $ V2 x y)
          ) charSurface'
        --saveBMP ((\(Surface ptr _)->ptr) surface) =<< newCString "textFont.bmp"
        --(Right img) <- readImage "textFont.bmp"
        --savePngImage "textFont.png" img
        tex <- genObjectName :: IO TextureObject
        textureBinding Texture2D $= Just tex
        textureFilter Texture2D $= ( (Linear', Just Linear'), Linear')
        textureWrapMode Texture2D S $= (Repeated,Repeat)
        textureWrapMode Texture2D T $= (Repeated,Repeat)
        lockSurface surface
        (SDL.Raw.Types.Surface _ _ _ ptr _ _ _) <- peek ((\(Surface ptr _)->ptr) surface)

        --ptr <- surfacePixels surface
        texImage2D Texture2D NoProxy 0 RGBA8 (TextureSize2D (fromIntegral side') (fromIntegral side')) 0 (PixelData RGBA UnsignedByte ptr)
        unlockSurface surface
        generateMipmap' Texture2D
        flush
        textureBinding Texture2D $= Nothing
        --tex <- imageToTexture (convertRGBA8 img)
        freeSurface surface
        ls <- SDL.Font.lineSkip font
        free font
        (coods,glphs,_) <- foldM (\(c,m,i) ((char,(g,s)),(x',y',w',h')) -> do
            let n x = fromIntegral x / fromIntegral side ::Float
                (x,y,w,h) = (n x',n y',n w',n h')
                lst' = c . (\lst ->  x:y+h:x+w:y+h:x+w:y:x:y:lst)
                m' = m & at char .~ Just (g & index.~i)
            freeSurface s
            return (lst',m',i+1)
          ) (id,Map.empty,0) charSurface'
        buffer <- listToBuffer 2 StaticDraw ToFloat Float (coods [])
        (mat,r') <- get "Simple" r
        let mat' = mat & textureVals %~ (("uDiffuse",Renderer.Texture (name++"fontTexture") tex):)
        let font = Renderer.Font name mat' glphs buffer (fst range) ls
        return (font, r' & fonts.at nm.~Just font)
  subResources a = [RO (\r-> return (a^.fmaterial,r))]
  bind r Renderer.Font{_fmaterial=m} = bind r m
