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
module Game.Grid.GridWorld.CameraCommand
  (
    CameraCommand (..),

    camcmdTurn,
    camcmdTurnAdd,
    camcmdView,
    camcmdViewAdd,
    camcmdTurnView,
    camcmdTurnAddView,
    camcmdTurnViewAdd,
    camcmdTurnAddViewAdd,

  ) where

import MyPrelude
import Game.MEnv
import Game.Data.View
import Game.Grid.GridWorld.Turn


data CameraCommand =
    CameraCommand
    {
        cameracommandTurn :: !Turn,
        cameracommandTurnSpeed :: !Float,
        cameracommandTurnIsAdd :: !Bool,
        cameracommandView :: !View,
        cameracommandViewSpeed :: !Float,
        cameracommandViewIsAdd :: !Bool,
        cameracommandTicks :: !TickT
    }




--------------------------------------------------------------------------------
--  

camcmdTurn :: TickT -> Float -> Turn -> CameraCommand
camcmdTurn ticks speed turn = 
    CameraCommand
    {
        cameracommandTurn = turn,
        cameracommandTurnSpeed = speed,
        cameracommandTurnIsAdd = False,

        cameracommandView = mempty,
        cameracommandViewSpeed = 0.0,
        cameracommandViewIsAdd = True,

        cameracommandTicks = ticks
    }


camcmdTurnAdd :: TickT -> Float -> Turn -> CameraCommand
camcmdTurnAdd ticks speed turn = 
    CameraCommand
    {
        cameracommandTurn = turn,
        cameracommandTurnSpeed = speed,
        cameracommandTurnIsAdd = True,

        cameracommandView = mempty,
        cameracommandViewSpeed = 0.0,
        cameracommandViewIsAdd = True,
        
        cameracommandTicks = ticks
    }


camcmdView :: TickT -> Float -> View -> CameraCommand
camcmdView ticks speed view = 
    CameraCommand
    {
        cameracommandTurn = mempty,
        cameracommandTurnSpeed = 0.0,
        cameracommandTurnIsAdd = True,

        cameracommandView = view,
        cameracommandViewSpeed = speed,
        cameracommandViewIsAdd = False,
        
        cameracommandTicks = ticks
    }


camcmdViewAdd :: TickT -> Float -> View -> CameraCommand
camcmdViewAdd ticks speed view = 
    CameraCommand
    {
        cameracommandTurn = mempty,
        cameracommandTurnSpeed = 0.0,
        cameracommandTurnIsAdd = True,

        cameracommandView = view,
        cameracommandViewSpeed = speed,
        cameracommandViewIsAdd = True,
        
        cameracommandTicks = ticks
    }


camcmdTurnView :: TickT -> Float -> Turn -> Float -> View -> CameraCommand
camcmdTurnView ticks tspeed turn vspeed view = 
    CameraCommand
    {
        cameracommandTurn = turn,
        cameracommandTurnSpeed = tspeed,
        cameracommandTurnIsAdd = False,
        
        cameracommandView = view,
        cameracommandViewSpeed = vspeed, 
        cameracommandViewIsAdd = False,
        
        cameracommandTicks = ticks
    }


camcmdTurnAddView :: TickT -> Float -> Turn -> Float -> View -> CameraCommand
camcmdTurnAddView ticks tspeed turn vspeed view = 
    CameraCommand
    {
        cameracommandTurn = turn,
        cameracommandTurnSpeed = tspeed,
        cameracommandTurnIsAdd = True,
        
        cameracommandView = view,
        cameracommandViewSpeed = vspeed,
        cameracommandViewIsAdd = False,

        cameracommandTicks = ticks
    }


camcmdTurnViewAdd :: TickT -> Float -> Turn -> Float -> View -> CameraCommand
camcmdTurnViewAdd ticks tspeed turn vspeed view = 
    CameraCommand
    {
        cameracommandTurn = turn,
        cameracommandTurnSpeed = tspeed,
        cameracommandTurnIsAdd = False,
        
        cameracommandView = view,
        cameracommandViewSpeed = vspeed,
        cameracommandViewIsAdd = True,

        cameracommandTicks = ticks
    }


camcmdTurnAddViewAdd :: TickT -> Float -> Turn -> Float -> View -> CameraCommand
camcmdTurnAddViewAdd ticks tspeed turn vspeed view = 
    CameraCommand
    {
        cameracommandTurn = turn,
        cameracommandTurnSpeed = tspeed,
        cameracommandTurnIsAdd = True,
        
        cameracommandView = view,
        cameracommandViewSpeed = vspeed,
        cameracommandViewIsAdd = True,
        
        cameracommandTicks = ticks
    }

