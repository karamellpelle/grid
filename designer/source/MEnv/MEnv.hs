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
--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FunctionalDependencies #-}

module MEnv
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

    runMEnv,
    
  ) where



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
runMEnv :: EnvInit -> ((res -> LoadM a) -> LoadM a) -> (MEnv res a) -> IO (Maybe a)
runMEnv init withLoadedResource ma =
    tryLoading $ do
      withLoadedScreen (initScreen init) $ \screen -> do
        withLoadedSound (initSound init) $ \sound -> do
          withLoadedTick (initTick init) $ \tick -> do
            withLoadedKeys (initKeys init) $ \keys -> do 
              withLoadedForeign (initForeign init) $ \foreign -> do
                withLoadedFriends (initFriends init) $ \friends -> do
                  withLoadedPlatform (initPlatform init) $ \platform -> do
                    
                    -- my resource
                    withLoadedResource $ \res -> do
                      
                      let env = Env { envScreen = screen,
                                      envSound = sound,
                                      envTick = tick,
                                      envKeys = keys,
                                      envForeign = foreign,
                                      envFriends = friends,
                                      envPlatform = platform,
                                      envResource = res }

                      -- if loaders did not escape, run 'ma'
                      liftIO $ MState.evalStateT (menvUnwrap ma) env
        


--------------------------------------------------------------------------------
--  MEnvInside



class MonadIO m => (MEnvInside res m) | m -> res where 
    liftMEnv :: (MEnv res) a -> m a

instance (MEnvInside res (MEnv res)) where
    liftMEnv = id

instance (Monad m, MEnvInside res m) => MEnvInside res (MState.StateT s m) where
    liftMEnv = lift . liftMEnv






