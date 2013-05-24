-- grid is a game written in Haskell
-- Copyright (C) 2013 Carl Joachim Svenn
-- 
-- This file is part of grid.
-- 
-- grid is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- grid is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with grid.  If not, see <http://www.gnu.org/licenses/>.
--
module MEnv.Keys.GLFW
  (
    
    keysBegin,
    keysEnd,

    Position (..),
    
    keysKeyOnce,
    keysTouchHandleTouched,
    keysTouchHandleReleased,
    keysTouchHandlePointVector,
    keysTouchHandlePointVectorEnd,
    keysTouchHandleCircleVector,
    keysTouchHandleCircleVectorEnd,
    keysTouchHandleButtonA,
    keysTouchHandleButtonB,

  ) where


import MyPrelude
import MEnv
import MEnv.Helpers
import MEnv.Tick
import Data.IORef

-- tmp:
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW



type Position =
    (Float, Float)


-- | button B press => 'ticksButtonB' <= hold time
ticksButtonB = 1.0


keysKeyOnce key = do
    getKeysObject >>= \obj -> io $ do
        down <- readIORef $ keysobjOnceDown obj
        case down of
            False   -> return False
            True    -> getkey key
{-
    io $ do
        n <- readIORef $ keysobjOnceRef obj
        case n of
            0   -> getkey key
            1   -> getkey key
            _   -> return False
-}
    where
      getkey key = 
          getKey key >>= \state -> case state of
              Release -> return False
              Press   -> return True

-- | begin iterate
keysBegin :: MEnv res ()
keysBegin = do
    tick' <- tickGet
    GL.Position x y <- io $ GL.get GLFW.mousePos
    GL.Size w h <- io $ GL.get GLFW.windowSize
    left <- io $ getButton GLFW.ButtonLeft
    right <- io $ getButton GLFW.ButtonRight
    let touch = if left || right then Just (fI x / fI w, 1.0 - fI y / fI h)
                                 else Nothing

    modifyKeysObject $ \obj -> case keysobjTouching obj of
        False   -> case touch of
            Nothing   -> obj { 
                                keysobjTouched = False,
                                keysobjReleased = False,
                                keysobjButtonB = False,
                                keysobjButtonA = False,
                                keysobjIsCircle = right }
          
            Just pos' -> obj { keysobjTouching = True,
                               keysobjTouchingTick = tick',
                               keysobjTouchingPos = pos',
                               keysobjTouchedTick = tick',
                               keysobjTouchedPos = pos',
                               keysobjTouched = True,
                               keysobjReleased = False,
                               keysobjButtonA = False,
                               keysobjButtonB = False,
                               keysobjButtonBHandled = False, 
                               keysobjIsCircle = right }

        True    -> case touch of
            Nothing   -> obj { keysobjTouching = False,
                               keysobjTouched = False,
                               keysobjReleased = True,
                               keysobjButtonA = not $ keysobjButtonBHandled obj,
                               keysobjButtonB = False,
                               keysobjIsCircle = right }

            Just pos'  -> if keysobjTouchedTick obj + ticksButtonB <= tick'
                            then obj { keysobjTouchingTick = tick',
                                       keysobjTouchingPos = pos',
                                       keysobjTouched = False,
                                       keysobjReleased = False,
                                       keysobjButtonB = not $ keysobjButtonBHandled obj,
                                       keysobjButtonBHandled = True,
                                       keysobjIsCircle = right }

                            else obj { keysobjTouchingTick = tick',
                                       keysobjTouchingPos = pos',
                                       keysobjTouched = False,
                                       keysobjReleased = False,
                                       keysobjIsCircle = right }

    where
      getButton but =
          GLFW.getMouseButton but >>= \state -> case state of
              GLFW.Release    -> return False
              GLFW.Press      -> return True

-- | end iterate
keysEnd :: MEnv res ()
keysEnd = do
    getKeysObject >>= \obj -> io $ do
        modifyIORef (keysobjOnceDown obj) $ const False

    return ()


--------------------------------------------------------------------------------
--  touch


-- | first finger down
keysTouchHandleTouched :: a -> (Position -> a) -> MEnv res a
keysTouchHandleTouched a f = do
    getKeysObject >>= \obj -> case keysobjTouched obj of
        False   -> return a
        True    -> return $ f $ keysobjTouchedPos obj


-- | last finger up
keysTouchHandleReleased :: a -> (Position -> a) -> MEnv res a
keysTouchHandleReleased a f =
    getKeysObject >>= \obj -> case keysobjReleased obj of
        False   -> return a
        True    -> return $ f (keysobjTouchingPos obj)


-- | one finger drag. cancelled by Circle (fixme)
keysTouchHandlePointVector :: a -> (TickT -> Position -> Position -> a) -> MEnv res a
keysTouchHandlePointVector a f = 
    getKeysObject >>= \obj -> case keysobjTouching obj && not (keysobjIsCircle obj) of
        False   -> return a
        True    -> return $ f (keysobjTouchingTick obj - keysobjTouchedTick obj)
                              (keysobjTouchedPos obj) (keysobjTouchingPos obj)


-- | one finger drop. cancelled by Circle.
keysTouchHandlePointVectorEnd :: a -> (TickT -> Position -> Position -> a) -> MEnv res a
keysTouchHandlePointVectorEnd a f =
    getKeysObject >>= \obj -> case keysobjReleased obj && not (keysobjIsCircle obj) of
        False   -> return a
        True    -> return $ f (keysobjTouchingTick obj - keysobjTouchedTick obj)
                              (keysobjTouchedPos obj) (keysobjTouchingPos obj)
               

-- | two finger drag. position is center. (fixme)
keysTouchHandleCircleVector :: a -> (TickT -> Position -> Float -> Position -> Float -> a) -> MEnv res a
keysTouchHandleCircleVector a f = 
    getKeysObject >>= \obj -> case keysobjTouching obj && keysobjIsCircle obj of
        False   -> return a
        True    -> return $ f 
            (keysobjTouchingTick obj - keysobjTouchedTick obj) 
            (keysobjTouchedPos obj) 0.0
            (keysobjTouchingPos obj) (radius (keysobjTouchingPos obj) (keysobjTouchedPos obj))
    where
      radius (x0, y0) (x1, y1) =
          y1 - y0


-- | two finger drop. position is center. (fixme)
keysTouchHandleCircleVectorEnd :: a -> (TickT -> Position -> Float -> Position -> Float -> a) -> MEnv res a
keysTouchHandleCircleVectorEnd a f =
    getKeysObject >>= \obj -> case keysobjReleased obj && keysobjIsCircle obj of
        False   -> return a
        True    -> return $ f
            (keysobjTouchingTick obj - keysobjTouchedTick obj) 
            (keysobjTouchedPos obj) 0.0
            (keysobjTouchingPos obj) (radius (keysobjTouchingPos obj) (keysobjTouchedPos obj))
    where
      radius (x0, y0) (x1, y1) =
          y1 - y0




-- | finger touched and release with time strictly less than 'ticksButtonB'. cancelled by Circle (fixme).
keysTouchHandleButtonA :: a -> (Position -> a) -> MEnv res a
keysTouchHandleButtonA a f = 
    getKeysObject >>= \obj -> case keysobjButtonA obj of
        False   -> return a
        True    -> return $ f (keysobjTouchedPos obj)


-- | finger touched and hold with time greater or equal to 'ticksButtonB'. cancelled by Circle (fixme).
keysTouchHandleButtonB :: a -> (Position -> a) -> MEnv res a
keysTouchHandleButtonB a f = 
    getKeysObject >>= \obj -> case keysobjButtonB obj of
        False   -> return a
        True    -> return $ f (keysobjTouchedPos obj)





