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
module OpenGL.Shade
  (
    tex0,
    tex1,
    tex2,
    tex3,
    attVec0,
    attVec1,
    attVec2,
    attVec3,
    attCoord0,
    attCoord1,
    attCoord2,
    attCoord3,

    attPos,
    attNormal,
    attColor,
    attTexCoord,
    attStencilCoord,

  ) where

import OpenGL



--------------------------------------------------------------------------------
--  textures

tex0 :: GLuint
tex0 = 0

tex1 :: GLuint
tex1 = 1

tex2 :: GLuint
tex2 = 2

tex3 :: GLuint
tex3 = 3




--------------------------------------------------------------------------------
--  vectors

attVec0 :: GLuint
attVec0 = 0

attVec1 :: GLuint
attVec1 = 1

attVec2 :: GLuint
attVec2 = 2

attVec3 :: GLuint
attVec3 = 3



--------------------------------------------------------------------------------
--  coordinates


attCoord0 :: GLuint
attCoord0 = 4

attCoord1 :: GLuint
attCoord1 = 5

attCoord2 :: GLuint
attCoord2 = 6

attCoord3 :: GLuint
attCoord3 = 7



--------------------------------------------------------------------------------
--  special

attPos :: GLuint
attPos = attVec0

attNormal :: GLuint
attNormal = attVec1

attColor :: GLuint
attColor = attVec2

attTexCoord :: GLuint
attTexCoord = attCoord0

attStencilCoord :: GLuint
attStencilCoord = attCoord1

