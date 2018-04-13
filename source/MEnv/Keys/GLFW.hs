-- grid is a game written in Haskell
-- Copyright (C) 2018 karamellpelle@hotmail.com
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
{-# LANGUAGE ForeignFunctionInterface #-}
module MEnv.Keys.GLFW
  ( 
    keysClear,

    Position,

    keysTouchHandlePointTouched,
    keysTouchHandlePointReleased,
    keysTouchHandlePointDrag,
    keysTouchHandlePointDrop,
    keysTouchHandleCircleTouched,
    keysTouchHandleCircleReleased,
    keysTouchHandleCircleDrag,
    keysTouchHandleCircleDrop,
    keysTouchHandleButtonA,
    keysTouchHandleButtonB,

    keysAcclGyro,

  ) where


import MyPrelude
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import MEnv
import MEnv.Tick



-- todo: custom type + bang patterns
type Position =
    (Float, Float)


--------------------------------------------------------------------------------
--  

-- | void glfw_keysClear()
foreign import ccall unsafe "glfw_keysClear" glfw_keysClear
    :: IO ()


-- | reset keys
keysClear :: MEnv res ()
keysClear = io $ 
    glfw_keysClear



--------------------------------------------------------------------------------
--  finger functions
--  * sequences consisting of 1 fingers are called Point sequences
--  * sequences consisting of 2 fingers are called Circle sequences
--    (but has actuallly more structure than a Circle, also angle)


-- | uint glfw_keysTouchHandlePointTouched(float* , float* )
foreign import ccall unsafe "glfw_keysTouchHandlePointTouched" 
    glfw_keysTouchHandlePointTouched
    :: Ptr CFloat -> Ptr CFloat -> IO CUInt

-- fixme: allocaArray
-- | first finger down
keysTouchHandlePointTouched :: a -> (Position -> a) -> MEnv res a
keysTouchHandlePointTouched a f = io $ do
    allocaArray 2 $ \ptr -> do
        let ptrX = advancePtr ptr 0
            ptrY = advancePtr ptr 1
        glfw_keysTouchHandlePointTouched ptrX ptrY >>= \value -> case value of
            0   -> return a
            _   -> do
              x <- peek ptrX
              y <- peek ptrY
              return $ f (realToFrac x, realToFrac y)



-- | uint glfw_keysTouchHandleReleased(float* , float* )
foreign import ccall unsafe "glfw_keysTouchHandlePointReleased" 
    glfw_keysTouchHandlePointReleased
    :: Ptr CFloat -> Ptr CFloat -> IO CUInt

-- fixme: allocaArray
-- | last finger up
keysTouchHandlePointReleased :: a -> (Position -> a) -> MEnv res a
keysTouchHandlePointReleased a f = io $ do
    allocaArray 2 $ \ptr -> do
        let ptrX = advancePtr ptr 0
            ptrY = advancePtr ptr 1
        glfw_keysTouchHandlePointReleased ptrX ptrY >>= \value -> case value of
            0   -> return a
            _   -> do
              x <- peek ptrX
              y <- peek ptrY
              return $ f (realToFrac x, realToFrac y)



-- | uint glfw_keysTouchHandlePointVector(double* ticks, float* x, float* y, float* x1, float* y1)
foreign import ccall unsafe "glfw_keysTouchHandlePointDrag" 
    glfw_keysTouchHandlePointDrag
    :: Ptr CDouble -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CUInt

-- fixme: allocaArray
-- | one finger drag. cancelled by two fingers (?)
keysTouchHandlePointDrag :: a -> (Tick -> Position -> Position -> a) -> MEnv res a
keysTouchHandlePointDrag a f = io $ do
    alloca $ \ptrTicks -> 
      allocaArray 4 $ \ptr -> do
          let ptrX0 = advancePtr ptr 0
              ptrY0 = advancePtr ptr 1
              ptrX1 = advancePtr ptr 2
              ptrY1 = advancePtr ptr 3
          glfw_keysTouchHandlePointDrag ptrTicks
                                           ptrX0
                                           ptrY0
                                           ptrX1
                                           ptrY1    >>= \value -> case value of
              0   -> return a
              _   -> do
                ticks <- peek ptrTicks
                x0 <- peek ptrX0
                y0 <- peek ptrY0
                x1 <- peek ptrX1
                y1 <- peek ptrY1
                return $ f (realToFrac ticks) (realToFrac x0, realToFrac y0)
                                              (realToFrac x1, realToFrac y1)



-- | uint glfw_keysTouchHandlePointVector(double* ticks, float* x, float* y, float* x1, float* y1)
foreign import ccall unsafe "glfw_keysTouchHandlePointDrop" glfw_keysTouchHandlePointDrop
    :: Ptr CDouble -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CUInt

-- fixme: allocaArray
-- | one finger drop. cancelled by two fingers (?)
keysTouchHandlePointDrop:: a -> (Tick -> Position -> Position -> a) -> MEnv res a
keysTouchHandlePointDrop a f = io $ 
    alloca $ \ptrTicks -> 
      allocaArray 4 $ \ptr -> do
          let ptrX0 = advancePtr ptr 0
              ptrY0 = advancePtr ptr 1
              ptrX1 = advancePtr ptr 2
              ptrY1 = advancePtr ptr 3
          glfw_keysTouchHandlePointDrop ptrTicks
                                           ptrX0
                                           ptrY0
                                           ptrX1
                                           ptrY1    >>= \value -> case value of
              0   -> return a
              _   -> do
                ticks <- peek ptrTicks
                x0 <- peek ptrX0
                y0 <- peek ptrY0
                x1 <- peek ptrX1
                y1 <- peek ptrY1
                return $ f (realToFrac ticks) (realToFrac x0, realToFrac y0)
                                              (realToFrac x1, realToFrac y1)               



-- | 
foreign import ccall unsafe "glfw_keysTouchHandleCircleTouched"
    glfw_keysTouchHandleCircleTouched
    :: Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CUInt

-- fixme: allocaArray
-- |
keysTouchHandleCircleTouched :: a -> (Position -> Float -> a) -> MEnv res a
keysTouchHandleCircleTouched a f = io $ 
    allocaArray 3 $ \ptr -> do
        let ptrX = advancePtr ptr 0
            ptrY = advancePtr ptr 1
            ptrRadius = advancePtr ptr 2
        glfw_keysTouchHandleCircleTouched ptrX 
                                             ptrY
                                             ptrRadius >>= \value -> case value of
            0   -> return a
            _   -> do
              x <- peek ptrX
              y <- peek ptrY
              radius <- peek ptrRadius
              return $ f (realToFrac x, realToFrac y) (realToFrac radius)



-- |
foreign import ccall unsafe "glfw_keysTouchHandleCircleReleased"
    glfw_keysTouchHandleCircleReleased
    :: Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CUInt

-- fixme: allocaArray
-- | 
keysTouchHandleCircleReleased :: a -> (Position -> Float -> a) -> MEnv res a
keysTouchHandleCircleReleased a f = io $ 
    allocaArray 3 $ \ptr -> do
        let ptrX = advancePtr ptr 0
            ptrY = advancePtr ptr 1
            ptrRadius = advancePtr ptr 2
        glfw_keysTouchHandleCircleReleased ptrX 
                                              ptrY
                                              ptrRadius >>= \value -> case value of
            0   -> return a
            _   -> do
              x <- peek ptrX
              y <- peek ptrY
              radius <- peek ptrRadius
              return $ f (realToFrac x, realToFrac y) (realToFrac radius) 



-- |
foreign import ccall unsafe "glfw_keysTouchHandleCircleDrag"
    glfw_keysTouchHandleCircleDrag
    :: Ptr CDouble -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> 
                      Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CUInt

-- | two finger drag. position is center.
keysTouchHandleCircleDrag :: a -> (Tick -> Position -> Float -> Position -> Float -> a) -> MEnv res a
keysTouchHandleCircleDrag a f = io $ 
    alloca $ \ptrTicks -> 
      allocaArray 6 $ \ptr -> do
          let ptrX0 = advancePtr ptr 0
              ptrY0 = advancePtr ptr 1
              ptrRadius0 = advancePtr ptr 2
              ptrX1 = advancePtr ptr 3
              ptrY1 = advancePtr ptr 4
              ptrRadius1 = advancePtr ptr 5
          glfw_keysTouchHandleCircleDrag ptrTicks
                                            ptrX0
                                            ptrY0
                                            ptrRadius0
                                            ptrX1
                                            ptrY1
                                            ptrRadius1 >>= \value -> case value of

              0   -> return a
              _   -> do
                ticks <- peek ptrTicks
                x0 <- peek ptrX0
                y0 <- peek ptrY0
                radius0 <- peek ptrRadius0
                x1 <- peek ptrX1
                y1 <- peek ptrY1
                radius1 <- peek ptrRadius1
                return $ f (realToFrac ticks)
                           (realToFrac x0, realToFrac y0) (realToFrac radius0)
                           (realToFrac x1, realToFrac y1) (realToFrac radius1)




-- |
foreign import ccall unsafe "glfw_keysTouchHandleCircleDrop"
    glfw_keysTouchHandleCircleDrop
    :: Ptr CDouble -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat ->
                      Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CUInt

-- fixme: allocaArray
-- | two finger drop. position is center.
keysTouchHandleCircleDrop :: a -> (Tick -> Position -> Float -> Position -> Float -> a) -> MEnv res a
keysTouchHandleCircleDrop a f = io $ 
    alloca $ \ptrTicks -> 
      allocaArray 6 $ \ptr -> do
          let ptrX0 = advancePtr ptr 0
              ptrY0 = advancePtr ptr 1
              ptrRadius0 = advancePtr ptr 2
              ptrX1 = advancePtr ptr 3
              ptrY1 = advancePtr ptr 4
              ptrRadius1 = advancePtr ptr 5
          glfw_keysTouchHandleCircleDrop ptrTicks
                                            ptrX0
                                            ptrY0
                                            ptrRadius0
                                            ptrX1
                                            ptrY1
                                            ptrRadius1 >>= \value -> case value of

              0   -> return a
              _   -> do
                ticks <- peek ptrTicks
                x0 <- peek ptrX0
                y0 <- peek ptrY0
                radius0 <- peek ptrRadius0
                x1 <- peek ptrX1
                y1 <- peek ptrY1
                radius1 <- peek ptrRadius1
                return $ f (realToFrac ticks)
                           (realToFrac x0, realToFrac y0) (realToFrac radius0)
                           (realToFrac x1, realToFrac y1) (realToFrac radius1)




foreign import ccall unsafe "glfw_keysTouchHandleButtonA"
    glfw_keysTouchHandleButtonA
    :: Ptr CFloat -> Ptr CFloat -> IO CUInt

-- fixme: allocaArray
-- | finger touched and release with time strictly less than 'ticksButtonB'. 
--   cancelled by Circle
keysTouchHandleButtonA :: a -> (Position -> a) -> MEnv res a
keysTouchHandleButtonA a f = io $ 
    allocaArray 2 $ \ptr -> do
        let ptrX = advancePtr ptr 0
            ptrY = advancePtr ptr 1
        glfw_keysTouchHandleButtonA ptrX ptrY >>= \value -> case value of
            0   -> return a
            _   -> do
              x <- peek ptrX
              y <- peek ptrY
              return $ f (realToFrac x, realToFrac y)




foreign import ccall unsafe "glfw_keysTouchHandleButtonB" glfw_keysTouchHandleButtonB
    :: Ptr CFloat -> Ptr CFloat -> IO CUInt

-- | finger touched and hold with time greater or equal to 'ticksButtonB'. 
--   cancelled by Circle.
keysTouchHandleButtonB :: a -> (Position -> a) -> MEnv res a
keysTouchHandleButtonB a f = io $ 
    allocaArray 2 $ \ptr -> do
        let ptrX = advancePtr ptr 0
            ptrY = advancePtr ptr 1
        glfw_keysTouchHandleButtonB ptrX ptrY >>= \value -> case value of
            0   -> return a
            _   -> do
              x <- peek ptrX
              y <- peek ptrY
              return $ f (realToFrac x, realToFrac y)




--------------------------------------------------------------------------------
--  AcclGyro functions

foreign import ccall unsafe "glfw_keysAcclGyro" glfw_keysAcclGyro
    :: Ptr CFloat -> Ptr CFloat -> Ptr CFloat ->
       Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CUInt


-- | state of Accel and Gyro
keysAcclGyro :: MEnv res (Float, Float, Float, Float, Float, Float)
keysAcclGyro = io $ do
    allocaArray 6 $ \ptr -> do
      glfw_keysAcclGyro (ptr `advancePtr` 0)
                       (ptr `advancePtr` 1)
                       (ptr `advancePtr` 2)
                       (ptr `advancePtr` 3)
                       (ptr `advancePtr` 4)
                       (ptr `advancePtr` 5)

      ax <- peekElemOff ptr 0
      ay <- peekElemOff ptr 1
      az <- peekElemOff ptr 2
      gx <- peekElemOff ptr 3
      gy <- peekElemOff ptr 4
      gz <- peekElemOff ptr 5
      return (rTF ax, rTF ay, rTF az, rTF gx, rTF gy, rTF gz)


