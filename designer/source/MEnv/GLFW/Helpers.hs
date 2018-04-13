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
module MEnv.Helpers
  (
    getScreenObject,
    modifyScreenObject,

    getSoundObject,
    modifySoundObject,

    getTickObject,
    modifyTickObject,

    getKeysObject,
    modifyKeysObject,

    getResource,
    modifyResource,

    liftIO,

    module MEnv.Env,

    
  ) where


import Control.Monad.State as MState
import MEnv
import MEnv.Env



--------------------------------------------------------------------------------
--  


getScreenObject :: MEnv res ScreenObject
getScreenObject =
    MState.get >>= \env -> return $ envScreen env


modifyScreenObject :: (ScreenObject -> ScreenObject) -> MEnv res ()
modifyScreenObject f =
    MState.modify $ \env -> env { envScreen = f $ envScreen env }



--------------------------------------------------------------------------------
--  


getSoundObject :: MEnv res SoundObject
getSoundObject =
    MState.get >>= \env -> return $ envSound env


modifySoundObject :: (SoundObject -> SoundObject) -> MEnv res ()
modifySoundObject f =
    MState.modify $ \env -> env { envSound = f $ envSound env }




--------------------------------------------------------------------------------
--  

getTickObject :: MEnv res TickObject
getTickObject =
    MState.get >>= \env -> return $ envTick env


modifyTickObject :: (TickObject -> TickObject) -> MEnv res ()
modifyTickObject f =
    MState.modify $ \env -> env { envTick = f $ envTick env }





--------------------------------------------------------------------------------
--  

getKeysObject :: MEnv res KeysObject
getKeysObject =
    MState.get >>= \env -> return $ envKeys env


modifyKeysObject :: (KeysObject -> KeysObject) -> MEnv res ()
modifyKeysObject f =
    MState.modify $ \env -> env { envKeys = f $ envKeys env }



--------------------------------------------------------------------------------
--  

getResource :: MEnv res res
getResource =
    MState.get >>= \env -> return $ envResource env


modifyResource :: (res-> res) -> MEnv res ()
modifyResource f =
    MState.modify $ \env -> env { envResource = f $ envResource env }


