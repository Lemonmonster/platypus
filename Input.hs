{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE GADTs#-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE Strict #-}
module Input where

import Prelude hiding ((.),id)
import Entity
import SDL hiding (delay)
import SDL.Input
import Control.Lens
import SDL.Video
import qualified Data.Map as M
import Control.Wire hiding (unless)
import Linear.V2
import Graphics.Rendering.OpenGL as GL
import Control.Monad
import System.Exit
import System.IO.Unsafe
import Renderer hiding (get)
import Geometry
import Debug.Trace

data Mouse = Mouse{_pressed::MouseButton -> Bool,_position::V2 Float}
makeLenses ''Mouse

newtype Keyboard = Keyboard (Scancode -> Bool)

newtype KeyBindings = KeyBindings (M.Map String Scancode)

newtype WindowSize = WS (V2 Int)

data Quit = Qu deriving (Show,Ord,Eq)

instance EntityW '[Delayed (Window,GLContext)] '[Quit] (Window,GLContext) '[] where
  wire =  mkGen_ ( \(lst `ECons` ENil,q `SCons` SNil) ->
    case lst of
      [] -> do
        initializeAll
        window <- createWindow "SDL Practice"
                                defaultWindow{windowInitialSize = V2 2560 1440,
                                              windowHighDPI = True,
                                              windowOpenGL = Just defaultOpenGL}
        gl <- glCreateContext window
        clearColor $= Color4 1 0 0 1
        swapInterval $= ImmediateUpdates
        return $ Right ([(window,gl)],SNil)
      D x :_ -> do
        --print q
        unless (null q) $ do
          destroyWindow $ fst x
          exitSuccess
        return $ Right ([x],SNil)
      )

instance EntityW '[(Window,GLContext)] '[]  Window '[] where
  wire = first ( arr (map fst . headE))

instance EntityW '[(Window,GLContext)] '[]  GLContext '[] where
  wire =  first ( arr (map snd . headE))

instance EntityW '[Window,Renderer.Renderer,SDL.Event] '[] Mouse '[] where
  wire =   first $ mkGen_ $ \(l `ECons` [r] `ECons` e `ECons` ENil)->  do
     (V2 w h) <- get $ windowSize (head l)
     (P (V2 x y)) <- getAbsoluteMouseLocation
     let weighted = (V2 (fromIntegral x/fromIntegral w) (fromIntegral y/fromIntegral h) ) - 0.5
         out = weighted `fillMul` (convertTransform $ (r^.Renderer.viewport.trans & Geometry.scale %~ (1/)))
     f <- getMouseButtons
     return $ Right [Input.Mouse f (out * V2 1 (-1))]


instance EntityW '[Window] '[] WindowSize '[] where
 wire =   first (mkGen_ $ \([w] `ECons` ENil) ->  do
    (V2 w h) <- get $ windowSize w
    return $ Right [WS (fromIntegral <$> V2 w h)]
    )

--uses window as a value dependency only
instance EntityW '[Window,SDL.Event] '[] Keyboard '[] where
  wire = first ( mkGen_ $ (const $ (return . Right . return . Keyboard)=<<getKeyboardState))

instance EntityW '[(Window,GLContext)] '[] SDL.Event '[Quit] where
  wire =mkGen_ $ \(_ `ECons` ENil,SNil) ->  do
      events <- pollEvents
      let isQuit (Event _ QuitEvent) = True
          isQuit _ = False
          doQuit = any isQuit events
      return $ Right (events,(if doQuit then [Signal Nothing Qu] else []) `SCons` SNil)
