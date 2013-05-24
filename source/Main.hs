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
module Main
  (
    main,

  ) where

import MyPrelude

#ifdef GRID_PLATFORM_IOS
import Main.IOS
#endif
#ifdef GRID_PLATFORM_GLFW
import Main.GLFW
#endif

#ifdef DEBUG
import OpenGL
import OpenGL.Helpers
import Foreign
#endif

main :: IO ()
main = do

#ifdef DEBUG
    -- we assume the following bitsizes in our code. 
    -- otherwise, the program will probably fail...
    assert (sizeOf (undefined :: GLubyte) == 1)   $ "sizeof GLubyte == 1"
    assert (sizeOf (undefined :: GLbyte) == 1)    $ "sizeof GLbyte == 1"
    assert (sizeOf (undefined :: GLushort) == 2)  $ "sizeof GLushort == 2"
    assert (sizeOf (undefined :: GLshort) == 2)   $ "sizeof GLshort == 2"
    assert (sizeOf (undefined :: GLfloat) == 4)   $ "sizeof GLfloat == 4"
#endif

    -- platform main
    main'









