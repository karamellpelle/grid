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
module MEnv.IOS
  (
    MEnv (..),
    runMEnvIOS,
    
    module MEnv.IOS.Init,

  ) where



import Foreign
import Foreign.Marshal.Alloc
import Data.IORef
import Control.Monad.Trans
import Control.Monad.State

import MEnv.IOS.Init


--------------------------------------------------------------------------------
--  MEnv

-- | the MEnv monad: computations inside an environment
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
--  runMEnvIOS


-- | init environment, then run MEnv inside. on iOS.
runMEnvIOS :: Init -> IO res -> (res -> IO ()) -> 
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

    -- set init
    alloca $ \ptr -> do
        poke ptr init
        ios_init ptr
    
    refEnv <- newIORef $ error "runIOSMEnv: refEnv undefined"
    refB <- newIORef $ error "runIOSMEnv: refB undefined"

    -- a -> m b
    -- create callback into Haskell from Foreign
    funptrBegin <- mkHaskellCall $ do
        -- create resource
        env <- loadResource

        (b, env') <- runStateT (menvUnwrap $ begin a) env
        writeIORef refB b
        writeIORef refEnv env'

    -- b -> m b
    -- create callback into Haskell from Foreign
    funptrIterate <- mkHaskellCall $ do
        env <- readIORef refEnv
        b <- readIORef refB
        (b', env') <- runStateT (menvUnwrap $ iterate b) env
        writeIORef refB b'
        writeIORef refEnv env'

    -- call Haskell from Foreign. this function should not return. 
    ios_main funptrBegin funptrIterate

    -- (the following code should not run in practice)

    freeHaskellFunPtr funptrIterate
    freeHaskellFunPtr funptrBegin

    -- b -> m c
    b <- readIORef refB
    env <- readIORef refEnv
    (c, env') <- runStateT (menvUnwrap $ end b) env

    unloadResource env'

    return c



-- | create 'void (*fun_ptr)()' of 'IO ()'
foreign import ccall "wrapper" mkHaskellCall
    :: IO () -> IO (FunPtr (IO ()))


-- | void ios_init(IOSInit* )
foreign import ccall "ios_init" ios_init
    :: Ptr Init -> IO ()

-- | void ios_main(void (*begin)(), void (*iterate)())
foreign import ccall safe "ios_main" ios_main
    :: FunPtr (IO ()) -> FunPtr (IO ()) -> IO ()







