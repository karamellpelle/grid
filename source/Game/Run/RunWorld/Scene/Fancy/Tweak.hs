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
module Game.Run.RunWorld.Scene.Fancy.Tweak
  (
    Tweak (..),
    makeTweak,

    module Game.Run.RunWorld.Scene.Fancy.Values,

  ) where

import MyPrelude
import Linear
import Game
import Game.Grid
import Game.Run.RunWorld.Scene.Fancy.Values



data Tweak =
    Tweak 
    {
        -- force
        tweakForAcc :: !Vec4,
        tweakForAccAlpha :: !Float,

        tweakForVel :: !Vec4,
        tweakForVelAlpha :: !Float,

        tweakForPos :: !Vec4,
        tweakForPosAlpha :: !Float,

        -- rotation
        tweakRotAcc :: !Vec4,
        tweakRotAccAlpha :: !Float,

        tweakRotVel :: !Vec4,
        tweakRotVelAlpha :: !Float,

        tweakRotPos :: !Vec4,
        tweakRotPosAlpha :: !Float,
        
        -- Osc0
        tweakOsc0Acc :: !Float,
        tweakOsc0Vel :: !Float,
        tweakOsc0Pos :: !Float,
        -- Osc1
        tweakOsc1Acc :: !Float,
        tweakOsc1Vel :: !Float,
        tweakOsc1Pos :: !Float,
        -- Osc2
        tweakOsc2Acc :: !Float,
        tweakOsc2Vel :: !Float,
        tweakOsc2Pos :: !Float,
        
        -- Pulse0
        tweakPulse0Vel :: !Float,
        tweakPulse0Pos :: !Float,
        -- Pulse1
        tweakPulse1Vel :: !Float,
        tweakPulse1Pos :: !Float,


        -- state
        tweakTick :: !Tick

#ifdef GRID_TEST
        , tweakConstI :: !Float
        , tweakConstJ :: !Float
        , tweakConstK :: !Float
        , tweakConstL :: !Float
#endif

    }




-- | create Tweako 
makeTweak :: Tweak
makeTweak = 
    Tweak
    {
        tweakForAcc = mempty,
        tweakForAccAlpha = 0.0,

        tweakForVel = mempty,
        tweakForVelAlpha = 0.0,

        tweakForPos = mempty,
        tweakForPosAlpha = 0.0,


        tweakRotAcc = mempty,
        tweakRotAccAlpha = 0.0,

        tweakRotVel = mempty,
        tweakRotVelAlpha = 0.0,

        tweakRotPos = mempty,
        tweakRotPosAlpha = 0.0,
        
        tweakOsc0Acc = 0.0,
        tweakOsc0Vel = 1.0,
        tweakOsc0Pos = 0.0,
        tweakOsc1Acc = 0.0,
        tweakOsc1Vel = 1.0,
        tweakOsc1Pos = 0.0,
        tweakOsc2Acc = 0.0,
        tweakOsc2Vel = 1.0,
        tweakOsc2Pos = 0.0,
        
        tweakPulse0Vel = 0.0,
        tweakPulse0Pos = 0.0,
        tweakPulse1Vel = 0.0,
        tweakPulse1Pos = 0.0,

        tweakTick = 0.0


#ifdef GRID_TEST_FOR
        , tweakConstI = valueTweakForI
        , tweakConstJ = valueTweakForJ
        , tweakConstK = valueTweakForK
        , tweakConstL = valueTweakForL
#endif
#ifdef GRID_TEST_ROT
        , tweakConstI = valueTweakRotI
        , tweakConstJ = valueTweakRotJ
        , tweakConstK = valueTweakRotK
        , tweakConstL = valueTweakRotL
#endif
#ifdef GRID_TEST_OSC0
        , tweakConstI = valueTweakOsc0I
        , tweakConstJ = valueTweakOsc0J
        , tweakConstK = valueTweakOsc0K
        , tweakConstL = valueTweakOsc0L
#endif
#ifdef GRID_TEST_PULSE0
        , tweakConstI = valueTweakPulse0I
        , tweakConstJ = valueTweakPulse0J
        , tweakConstK = valueTweakPulse0K
        , tweakConstL = valueTweakPulse0L
#endif

    }
    
