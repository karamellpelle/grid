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
module MEnv.Foreign.GLFW
  (
    foreignBeginForeign,
    foreignHandleForeignEnd,

  ) where

import MyPrelude
import MEnv


foreignBeginForeign :: MEnv res ()
foreignBeginForeign = io $ do
    putStrLn "GLFW.foreignBeginForeign"
    putStrLn "  -> here we go out of haskell and run other stuff. originally for other iOS GameCentral"



foreignHandleForeignEnd :: a -> a -> MEnv res a
foreignHandleForeignEnd a a' = io $ do
    putStrLn "GLFW.foreignHandleForeignEnd"
    putStrLn "  -> however, let's ignore foreign and go back to haskell"
    return a'


