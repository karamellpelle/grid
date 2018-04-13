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
module Test.Output
  (
    drawText,
      
  ) where


import MyPrelude
import Game

import Game.Data.Color
import Game.Font
import Text.Printf

import OpenGL
import OpenGL.Helpers

import OpenAL
import OpenAL.Helpers



-- | draw text
drawText :: GameData -> Float -> Float -> [ String ] -> IO ()
drawText gamedata wth hth strs = do
    let fsh = gamedataFontShade gamedata        
        ffd = gamedataFontData gamedata
    fontShade fsh 1.0 $ mat4Ortho2D 0 wth hth 0
    fontDrawDefault fsh ffd (valueTextFontCSize * hth) valueTextFontCColor 
    foldM_ (drawStr wth hth fsh ffd 0.01) 0.01 strs
    where
      drawStr wth hth fsh ffd x = \y str -> do
          fontDraw2D fsh ffd (x * wth) (y * hth) str
          return (y + valueTextFontCSize) 

