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
--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FunctionalDependencies #-}

module MEnv.GLFW
  (
    MEnv,
    MEnvInside (..),
    EnvInit (..),
    ScreenInit (..),
    SoundInit (..),
    TickInit (..),
    KeysInit (..),
    ForeignInit (..),
    FriendsInit (..),
    PlatformInit (..),

    runGLFWMEnv,
    
  ) where



import MyPrelude
import LoadM
import Control.Monad
import qualified Control.Monad.State as MState
import Control.Monad.Trans ()
import MEnv.Env



-- | the MEnv monad
newtype MEnv res a =
    MEnv
    {
        menvUnwrap :: MState.StateT (Env res) IO a
    } deriving (Monad,
                MonadIO,
                MState.MonadState (Env res),
                Functor)




-- | init environment, run (MEnv a) inside.
runGLFWMEnv :: EnvInit -> IO res -> (res -> IO ()) -> 
              (a -> MEnv res b) -> 
              (b -> MEnv res c) -> 
              a ->
              IO c
runGLFWMEnv init loadRes unloadRes begin fmc a =
    tryLoading' $ do
      withLoadedScreen (initScreen init) $ \screen -> do
        withLoadedSound (initSound init) $ \sound -> do
          withLoadedTick (initTick init) $ \tick -> do
            withLoadedKeys (initKeys init) $ \keys -> do 
              withLoadedForeign (initForeign init) $ \frgn -> do
                withLoadedFriends (initFriends init) $ \friends -> do
                  withLoadedPlatform (initPlatform init) $ \platform -> do
                   
                    io $ do
                        res <- loadRes
                        let env = Env { envScreen = screen,
                                        envSound = sound,
                                        envTick = tick,
                                        envKeys = keys,
                                        envForeign = frgn,
                                        envFriends = friends,
                                        envPlatform = platform,
                                        envResource = res }
                       
                         
                        (c, env') <- (flip MState.runStateT) env $ menvUnwrap $ do
                            b <- begin a
                            fmc b
                        unloadRes $ envResource env'
                        return c
                        
                          
        
      where
        tryLoading' :: LoadM a -> IO a
        tryLoading' ma =
            tryLoading ma >>= \maybeA -> case maybeA of
                Just a    -> return a
                Nothing   -> error "could not load (tryLoading')"


--------------------------------------------------------------------------------
--  MEnvInside



class MonadIO m => (MEnvInside res m) | m -> res where 
    liftMEnv :: (MEnv res) a -> m a

instance (MEnvInside res (MEnv res)) where
    liftMEnv = id

instance (Monad m, MEnvInside res m) => MEnvInside res (MState.StateT s m) where
    liftMEnv = lift . liftMEnv






