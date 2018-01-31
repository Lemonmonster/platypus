{-#LANGUAGE ExistentialQuantification#-}
{-#LANGUAGE DataKinds#-}
{-#LANGUAGE MultiWayIf#-}
{-#LANGUAGE TupleSections#-}
module Atlas where

import Graphics.Rendering.OpenGL hiding (Color,Texture,get,viewport)
import qualified Graphics.Rendering.OpenGL as GL
import Codec.Picture
import qualified Data.Map.Strict as Map
import Geometry
import Linear (V2,V4(..))
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
import Material
import Renderer hiding (material)
import Debug.Trace
import Data.Char (isSpace)

packImages :: (Show a,Num a,Ord a) => ((a,a)->(a,a))-> (a,a) -> [(b,(a,a))] -> ([(b,(a,a,a,a))],(a,a))
packImages grow d@(width,height) images =
  let images' = sortBy (flip $ comparing (\(_,(w,h))->w*h)) images
      fit (w,h) (_,_,w',h') = w'>=w && h'>= h
      insert (val,d@(w,h)) (lst,boxes) =
        let bs = filter (fit d) boxes
        in if null bs then
             Nothing
           else
             let b@(x,y,w',h') = head bs
                 boxes' = sortBy (comparing (\(_,_,w,h)-> w*h)) [(x,y+h,w',h'-h),(x+w,y,w'-w,h)] ++ tail bs
             in Just ((val,(x,y,w,h)):lst,boxes')
      r = fst <$> foldl' (\x y-> x >>= insert y) (Just ([],[(0,0,width,height)])) images
  in maybe (packImages grow (grow d) images) (,d) r

instance Resource Atlas where
  identifier a = ("atlas",a^.aName)
  get file r =
    let (path,name) = nameAndPath file texturePath
        atlas = r ^. atlases . at name
    in if isJust atlas then do
        return (fromJust atlas,r)
       else  do
        d <- getCurrentDirectory
        paths <- listDirectory path
        material <- return . filter (not . isSpace) =<< readFile (path++"/"++"material")
        animations <- mapM (\s -> do
                  let currPath = path++"/"++s
                  b <- doesDirectoryExist currPath
                  if b then do -- load directory
                    paths <- (liftM (filter (=~ ("^"++s++"\\.(?!dat)"))))  (return . traceShowId =<< listDirectory currPath)
                    image <- readImage (traceShowId $ currPath++"/"++head paths)
                    (case image of
                      (Right dimage) -> do
                        let i@(Image w h v) = convertRGBA8 dimage
                        dat <- (liftM $ splitOn " ") $  readFile (currPath++"/"++s++".dat")
                        let w' = (read $ head dat) :: Int
                        let h' = (read $ head $ tail dat) :: Int
                        let r  = read (dat !! 2) :: Float
                        if (not (mod w w' == 0 && mod h h' == 0)) then
                          error $ "illegal frame size: does not divide image dimensions: "++ s
                        else
                          let boxes = [(x,y,w',h') | x <- takeWhile (<w) [0,w'..], y<-takeWhile (<h) [0,h'..]]
                          in return (s,(r,boxes), i)
                      (Left e) ->
                          error ("Error loading atlas "++file++s ++ " " ++ e)
                      )
                  else do
                    image <-  readImage currPath
                    (case image of
                      (Right dimage) ->
                        let i@(Image w h v) = convertRGBA8 dimage
                        in return (s,(60,[(0,0,w,h)]), i)
                      (Left e) ->
                        error ("Error loading atlas "++file++s ++ " " ++ e)
                     )
          ) (filter (/="material") paths)
        let getSnd (_,x,_) = x
            frameds = concat (map  (snd . getSnd) animations)
            area = foldl' (+) 0 (map (\(_,_,w,h)-> w*h) frameds)
            side = 2 ^ (ceiling (log (fromIntegral area)/log 4)) -- nearest square with integer sides
            addFrame lst (x',y',w',h') =
              let norm a = fromIntegral a /fromIntegral side
                  (x,y,w,h) = (norm x',norm y',norm w',norm h') :: (Float,Float,Float,Float)
              in x:y+h:x+w:y+h:x+w:y:x:y:lst
            sprites = concat (map (\(s,a,im)-> snd $ mapAccumL (\i b@(_,_,w,h)->  ( i+1 , ((i+1,(s,b,im)),(w,h)) )) 0 (snd a)) animations)
            (positions,(side',_)) = packImages (\(x,y)->(x*2,y*2)) (side,side) sprites
            img = generateImage (\x y ->
              let sprite = find (\(_,(x',y',w,h))-> x>x' && x< x'+w && y>y' && y<y'+h ) positions
              in case sprite of
                  (Just ((_,(_,(x'',y'',_,_),im)),(x',y',w,h)))->
                    pixelAt im (x''+(x-x')) (y''+(y-y'))
                  Nothing ->
                    PixelRGBA8 1 0 0 0
              ) side' side'
            animationLists =
              groupBy (\((_,(name,_,_)),_) ((_,(name',_,_)),_) -> name == name') $
                sortBy (\((indx,(name,_,_)),_) ((indx',(name',_,_)),_)->
                    compare name name' <> compare indx indx'
                  ) positions
            ((frams,_),indexLists) =
              mapAccumL (\(lst,i) animation->
                  mapAccumL (\(lst,i) ((indx,(n,_,_)),frame) ->((addFrame lst frame,i-1),(n,indx,i)) ) (lst,i) animation
                ) ([], foldl' (\a b-> a + length b) 0 animationLists - 1) animationLists
            animationAssocList = map (\(s,(frameRate,_),_) ->
                  let (Just list) = find (\((n,_,_):xs) -> n==s) indexLists
                  in (s,Animation frameRate (VG.fromList (map (\(_,i,_)->i) (sortBy (comparing (\(i,_,_)-> i)) list) )))
                ) animations
        buffer <- listToBuffer 2 StaticDraw ToFloat Float frams
        print $ length frams
        writePng (name++".png") img
        tex@(Renderer.Texture tname obj) <- return . Renderer.Texture (name++"AtlasTex") =<< imageToTexture img
        let r' = r & textures . at tname .~ Just tex
        ((Material mname texs uniforms attribs shader),r'') <- Renderer.get material r'
        let out = Atlas name (Material mname (("uDiffuse",tex):texs) uniforms attribs shader) (Map.fromList animationAssocList) buffer
        return (out,r'' & atlases . at name .~ Just out)
  bind r Atlas{_aMaterial = m} = bind r m
  subResources a = [RO (get (a^.aMaterial.mName) :: Getter Material)]
