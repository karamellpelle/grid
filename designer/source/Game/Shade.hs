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
module Game.Shade
  (
    attPos,
    attNormal,
    attColor,

    attTexCoord,
    attStencilCoord,
    attCoord0,
    attCoord1,
    attCoord2,

    tex0,
    tex1,
    tex2,

  ) where

import OpenGL



attPos :: GLuint
attPos = 0

attNormal :: GLuint
attNormal = 1

attColor :: GLuint
attColor = 2

attTexCoord :: GLuint
attTexCoord = 4

attStencilCoord :: GLuint
attStencilCoord = 5

attCoord0 :: GLuint
attCoord0 = 8

attCoord1 :: GLuint
attCoord1 = 9

attCoord2 :: GLuint
attCoord2 = 10


tex0 :: GLuint
tex0 = 0

tex1 :: GLuint
tex1 = 1

tex2 :: GLuint
tex2 = 2


