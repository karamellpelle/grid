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
module MEnv.Env.KeysObject.GLFW
  (
    KeysInit (..),
    KeysObject (..),
    
    withLoadedKeys,

  ) where


import LoadM




data KeysInit =
    KeysInit


data KeysObject =
    KeysObject
    {
        keysobjTouching :: Bool,
        keysobjTouchingTick :: TickT,
        keysobjTouchingPos :: Position,
        keysobjTouchedTick :: TickT,
        keysobjTouchedPos :: Position,

        keysobjTouched :: Bool,
        keysobjReleased :: Bool,
        keysobjButtonA :: Bool,
        keysobjButtonB :: Bool,
        keysobjButtonBHandled :: Bool

    }


type Position =
    (Double, Double)


type TickT =
    Double


--------------------------------------------------------------------------------
--  


-- | loading a Key object.
withLoadedKeys :: KeysInit -> (KeysObject -> LoadM a) -> LoadM a
withLoadedKeys init handler = do
    handler $ KeysObject { keysobjTouching = False,
                           keysobjTouchingTick = 0.0,
                           keysobjTouchingPos = (0.0, 0.0),
                           keysobjTouchedTick = 0.0,
                           keysobjTouchedPos = (0.0, 0.0),

                           keysobjTouched = False,
                           keysobjReleased = False,
                           keysobjButtonA = False,
                           keysobjButtonB = False,
                           keysobjButtonBHandled  = False
                           --keysobjButtonA = Nothing,
                           --keysobjButtonB = Nothing,
                           --keysobjButtonBHandled = False
                         }
                           
