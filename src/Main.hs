{-#LANGUAGE OverloadedStrings#-}
module Main where

import Network
import Control.Wire
import Control.Monad
import Control.Monad.Fix
import Control.Applicative
import SDL
import Graphics.Rendering.OpenGL as GL
import Linear.V2
import Foreign.C.Types
import System.IO.Unsafe

main :: IO ()
main = do
  mainLoop network clockSession_

  {-v@(w,s)<-mfix (\(~(w,s)) ->
       return w >>=
       (\w -> unsafeInterleaveIO $ get $ windowSize w) >>=
       (\x -> unsafeInterleaveIO $  do
        initializeAll
        window <- createWindow "SDL Practice" defaultWindow{windowInitialSize = V2 2560 1440, windowHighDPI = True, windowOpenGL = Just defaultOpenGL}
        gl <- glCreateContext window
        --clearColor $= Color4 1 0 0 1
        return (window,x))
    )-}

  --print $ v
  return ()


mainLoop w s = do
  (ds,s') <- stepSession s
  (v,w') <- stepWire w ds (Right ())
  w `seq` v `seq` mainLoop w' s'
