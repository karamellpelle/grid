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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module MEnv.GLFW
  (
    MEnv (..),
    runMEnvGLFW,
    
    module MEnv.GLFW.Init,

  ) where



import Foreign
import Foreign.Marshal.Alloc
import Data.IORef
import Control.Monad.Trans
import Control.Monad.State
import MyPrelude

import MEnv.GLFW.Init


--------------------------------------------------------------------------------
--  MEnv

-- | the MEnv monad
newtype MEnv res a =
    MEnv
    {
        menvUnwrap :: StateT res IO a
    }
    deriving
      (
          Applicative,
          Monad,
          MonadIO,
          MonadState res,
          Functor
      )



--------------------------------------------------------------------------------
--  runMEnvGLFW


-- | init environment, run (MEnv a) inside, from GLFW
runMEnvGLFW :: Init -> IO res -> (res -> IO ()) -> 
              (a -> MEnv res b) -> 
              (b -> MEnv res b) -> 
              (b -> MEnv res c) -> 
              a ->
              IO c
runMEnvGLFW init loadResource unloadResource
           begin
           iterate -- ignored
           end
           a = do

    -- initialize GLFW environment
    alloca $ \ptr -> do
        poke ptr init
        glfw_init ptr
    
    -- create GameData
    env <- loadResource

    -- run the game
    (c, env') <- runStateT (menvUnwrap $ game a) env

    -- clean up GameData
    unloadResource env'

    return c

    where
      game =
          begin >=>         -- a -> MEnv' b
          iterate' >=>      -- b -> MEnv' b
          end               -- b -> MEnv' c
          
      -- "main loop"
      iterate' = \b -> do
          io $ glfw_frame_begin
          b' <- iterate b
          io $ glfw_frame_end
          
          -- forever young
          iterate' b'

      --iterate' = \abstack@(a, b, stack) ->
      --    case stack of 
      --        []      -> return abstack
      --        (i:is)  -> do
      --            -- iterate inside GLFW frame
      --            io $ glfw_frame_begin
      --            (a', b', top) <- (iteration i) a b
      --            io $ glfw_frame_end
      --
      --            iterate' (a', b', top ++ is)


-- | void ios_init(IOSInit* )
foreign import ccall unsafe "glfw_init" glfw_init
    :: Ptr Init -> IO ()

foreign import ccall unsafe "glfw_frame_begin" glfw_frame_begin
    :: IO ()

foreign import ccall unsafe "glfw_frame_end" glfw_frame_end
    :: IO ()

