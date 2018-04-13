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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module MEnv.MEnv
  (
    MEnv,
    MEnvInside (..),

    menvUnwrap,
    
  ) where


import qualified Control.Monad.State as MState
import Control.Monad.Trans
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


--------------------------------------------------------------------------------
--  MEnvInside



class MonadIO m => (MEnvInside res m) | m -> res where 
    liftMEnv :: (MEnv res) a -> m a

instance (MEnvInside res (MEnv res)) where
    liftMEnv = id

instance (Monad m, MEnvInside res m) => MEnvInside res (MState.StateT s m) where
    liftMEnv = lift . liftMEnv






