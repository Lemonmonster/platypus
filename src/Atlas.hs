{-#LANGUAGE ExistentialQuantification#-}
{-#LANGUAGE DataKinds#-}
{-#LANGUAGE MultiWayIf#-}
{-#LANGUAGE TupleSections#-}
module Atlas where

import Graphics.Rendering.OpenGL hiding (Color,Texture,get,viewport)
import qualified Graphics.Rendering.OpenGL as GL
import Codec.Picture
import Codec.Picture.Types
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
import Data.Bool
import qualified Data.ByteString as B
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import qualified Data.Set as S
import CerealPlus.Serialize
import CerealPlus.Deserialize
import CerealPlus.Serializable

instance Serializable m PixelRGBA8 where
  serialize (PixelRGBA8 r b g a) =
    serialize (r,g,b,a)
  deserialize = do
    (\(r,g,b,a)-> PixelRGBA8 r g b a) <$> deserialize

packImages :: (Show a,Num a,Ord a) => ((a,a)->(a,a))-> (a,a) -> a -> [(b,(a,a))] -> ([(b,(a,a,a,a))],(a,a))
packImages grow d@(width,height) padding images =
  let mergeBy c l [] = l
      mergeBy c [] l = l
      mergeBy c (x:xs) (y:ys) = if c x y == LT
         then x : mergeBy c xs (y:ys)
         else y : mergeBy c (x:xs) ys
      compareF = comparing (\(_,_,w,h)-> w*h)
      images' = sortBy (flip $ comparing (\(_,(w,h))->w*h)) images
      fit (w,h) (_,_,w',h') = w'>=w+( padding*2 ) && h'>= h+( padding*2 )
      insert (val,d@(w,h)) (lst,boxes) =
        let bs = filter (fit d) boxes
        in if null bs then
             Nothing
           else
             let b@(x,y,w',h') = head bs
                 boxes' = 
                      (mergeBy compareF
                        ( sortBy compareF [(x,y+h+( padding*2 ),w',h'-(h + ( padding*2 ))),(x+w+( padding*2 ),y,w'-(w + ( padding*2 )),h + padding*2)] )
                        ( tail bs ))
             in Just ((val,(x+padding,y+padding,w,h)):lst,boxes')
      r = fst <$> foldl' (\x y-> x >>= insert y) (Just ([],[(0,0,width,height)])) images
  in maybe (trace "grow" $ packImages grow (grow d) padding images) (,d) r

instance Resource Atlas where
  identifier a = ("atlas",a^.aName)
  get file r =
    let (path,name) = nameAndPath file texturePath
        atlas = r ^. atlases . at name
    in if isJust atlas then do
        return (fromJust atlas,r)
       else  do
        exists <- doesDirectoryExist path
        unless exists $ error $ "Atlas "++name++" does not exist"
        d <- getCurrentDirectory
        paths <- listDirectory path
        material <- return . filter (not . isSpace) =<< readFile (path++"/"++"material")
        fmap fromJust . runMaybeT $ do
            unless (elem ".atlas" paths) $ do
              lift $ print "no cached atlas"
              mzero 
            let paths' = paths \\ [".atlas"]
            files <- fmap lines. lift . readFile $ path ++ "/.atlas/contents.data"
            unless (S.fromList paths' == S.fromList files) $ do
              lift $ print "atlas files added or removed"
              lift $ print $ "stored files: " ++ show files
              lift $ print $ "new files: " ++ show paths'
              mzero -- if anything as added or removed
            atlasTime <- lift . getModificationTime $ path ++ "/.atlas"
            otherTimes <- mapM (lift . getModificationTime) $ map (( path++"/" )++) paths'
            unless (all (<atlasTime) otherTimes) $ do
              lift $ print $ "atlas files changed: " ++
                              show ( map fst $ filter ((>=atlasTime).snd) $ zip paths' otherTimes)
              mzero
            lift.print $ "reading "++name
            result <- runPartial deserialize
              =<< (lift . B.readFile $ path ++ "/.atlas/atlas.bin")
            lift $ print "done"
            case result of
              Done (animations,frams) _ -> do
                image <-  do
                  img <- lift.readTiff $ path ++ "/.atlas/atlas.tiff"
                  case img of
                    (Right dimage) ->
                      return $ convertRGBA8 dimage
                    (Left e) -> do
                      lift $ print "failed to read cached atlas"
                      mzero
                buffer <- lift $ listToBuffer 2 StaticDraw ToFloat Float ( frams :: [Float])
                lift.print $ "loading texture: " ++ name
                tex@(Renderer.Texture tname obj) <- lift $ return . Renderer.Texture (name++"AtlasTex") =<< imageToTexture image
                lift $ print "done"
                let r' = r & textures . at tname .~ Just tex 
                ((Material mname texs uniforms attribs shader),r'') <- lift $ Renderer.get material r'
                let out = Atlas name
                                (Material mname (("uDiffuse",tex):texs) uniforms attribs shader)
                                animations
                                buffer
                return (out,r'' & atlases . at name .~ Just out)
              _ -> do
                lift . print $ "Error Reading Atlas: " ++ path
                mzero
          <|> lift (do
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
                        case image of
                          (Right dimage) ->
                            let i@(Image w h v) = convertRGBA8 dimage
                            in return (takeWhile (/='.') s,(60,[(0,0,w,h)]), i)
                          (Left e) ->
                            error ("Error loading atlas "++file++s ++ " " ++ e)
              ) (paths \\ [".atlas","material"])
            maxTextureSize' <- maxTextureSize
            maxImageUnits <- maxTextureImageUnits
            print $  "size and unit limits on textures" ++ show (maxTextureSize',maxImageUnits)
            let getSnd (_,x,_) = x
                frameds = concat (map  (snd . getSnd) animations)
                area = foldl' (+) 0 (map (\(_,_,w,h)-> w*h) frameds)
                side = 2 ^ (ceiling (log (fromIntegral area)/log 4)) -- nearest square with integer sides
                sprites = concat (snd $ mapAccumL (\i (s,a,im)-> mapAccumL (\i b@(_,_,w,h)->  ( i+1 , ((i,(s,b,im)),(w,h)) )) i (snd a)) 0 animations)
                padding = 2
                (positions,(side',_)) = packImages (\(x,y)->(x*2,y*2)) (side,side) padding sprites
                addFrame lst (x',y',w',h') =
                  let norm a = fromIntegral a /fromIntegral side'
                      (x,y,w,h) = (norm x',norm y',norm w',norm h') :: (Float,Float,Float,Float)
                  in x:y+h:x+w:y+h:x+w:y:x:y:lst
            --efficient drawing requires mutability
            imgMut <- createMutableImage side' side' (PixelRGBA8 0 0 0 0)
            mapM_ (\((_,(_,(x'',y'',_,_),im)),(x',y',w,h)) ->
                mapM_ (\(x,y) ->
                    writePixel imgMut x y (pixelAt im (x'' + min (w-1)  (max 0 (x - x')))  (y'' + min (h-1)  (max 0 (y - y')))) --stretches image into padding
                  ) [(x,y) | x<- [x'-padding..x'+w+padding] , y <- [y'-padding..y'+h+padding] ]
              ) positions
            img <- unsafeFreezeImage imgMut
            -- imgMut must not be referenced after here
            let animationLists =
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
                      in (s,Animation frameRate (VG.fromList (map (\(_,_,i)->i) (sortBy (comparing (\(_,i,_)-> i)) list) )))
                    ) $ traceShow indexLists $ animations
                animationMap = Map.fromList  animationAssocList
                premultipliedImage@(Image _ _ premultedPixels) = premultiply img
            print "atlas done"
            buffer <- listToBuffer 2 StaticDraw ToFloat Float frams
            createDirectoryIfMissing False (path ++ "/.atlas") 
            writeTiff (path ++ "/.atlas/atlas.tiff") img
            writeFile (path ++ "/.atlas/contents.data") (unlines $ paths \\ [ ".atlas" ])
            B.writeFile (path ++ "/.atlas/atlas.bin") =<< exec (serialize (animationMap,frams))
            tex@(Renderer.Texture tname obj) <- return . Renderer.Texture (name++"AtlasTex") =<< imageToTexture premultipliedImage
            print "atlas written"
            let r' = r & textures . at tname .~ Just tex 
            ((Material mname texs uniforms attribs shader),r'') <- Renderer.get material r'
            let out = Atlas name (Material mname (("uDiffuse",tex):texs) uniforms attribs shader) animationMap buffer
            return (out,r'' & atlases . at name .~ Just out))
  bind r Atlas{_aMaterial = m} = bind r m
  subResources a = [RO (get (a^.aMaterial.mName) :: Getter Material)]
