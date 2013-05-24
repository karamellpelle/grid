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
runMEnvIOS init loadResource unloadResource
           begin
           iterate
           end
           a = do
    putStrLn "GLFW is not implemented yet"
