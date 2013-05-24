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
module MEnv.Resource
  (
    resourceGet,
    resourceModify,
    resourceModifyM,

  ) where


import MEnv
import MEnv.Env
import MEnv.Helpers
import qualified Control.Monad.State as MState


-- | get resource
resourceGet :: MEnv res res
resourceGet = 
    getResource


-- | modify resource
resourceModify :: (res -> res) -> MEnv res ()
resourceModify =
    modifyResource


resourceModifyM :: (res -> IO res) -> MEnv res ()
resourceModifyM f = do
    env <- MState.get
    res' <- liftIO $ f $ envResource env
    MState.put $ env { envResource = res' }


