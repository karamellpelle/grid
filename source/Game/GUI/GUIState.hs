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
module Game.GUI.GUIState
  (
    GUIState (..),
    GUITick,
    GUIShape (..),
    GUIPos (..),

  ) where

import MyPrelude
import Game.GUI.GUIShade

import OpenGL
import OpenGL.Helpers

-- (fixme: verify this)
-- INVARIANTS WHEN DRAWING A NEW WIDGET
-- 
-- * GUIState resembles GL-state, except FillTex
-- * all textures used shall be upside down (consistent with 2D drawing (FillTex))
-- * Program == ShadeGUI
-- * DepthTest enabled
-- * DepthFunc == GL_LEQUAL
-- * StencilTest disabled
-- * FrontFace == GL_CCW ? (vs 3D)
-- * CullFace disabled  ? (vs 3D)
-- * y-direction is top to bottom



data GUIState =
    GUIState
    {
        -- time
        guistateTick :: !GUITick,
        
        -- GL
        guistateGUIShade :: GUIShade,
        guistateAlpha :: !Float,
        guistateProjModv :: !Mat4,
        guistateWth :: !Float,
        guistateHth :: !Float,

        -- (fixme: more general, ProjModv :: Mat4)
        guistatePos :: !GUIPos,
        guistateScaleX :: !Float,
        guistateScaleY :: !Float,

        guistateDepth :: !Float,
        guistateFocus :: !Float,
        guistateFillTex :: !GLuint

    }


-- | GUIShape's are relative to vertex xy-space
data GUIShape =
    GUIShape
    {
        shapeWth :: !Float,
        shapeHth :: !Float
    }

-- | GUIPos's are relative to vertex xy-space
data GUIPos =
    GUIPos
    {
        posX :: !Float,
        posY :: !Float
    }


type GUITick =
    Double


