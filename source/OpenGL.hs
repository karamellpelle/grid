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
module OpenGL
  (

#ifdef GRID_OPENGL_ES2
-- OpenGL ES 2.0
    module OpenGL.ES2,
    module OpenGL.ES2.Values,
    module OpenGL.ES2.Types,
#endif
#ifdef GRID_PLATFORM_IOS
    module OpenGL.ES2.ExtIOS,
#endif
#ifdef GRID_PLATFORM_GLFW
    module OpenGL.ES2.ExtGLFW,
#endif

    module Foreign.Storable,
    module Foreign.Ptr,
    module Foreign.C,
    module Foreign.Marshal.Alloc,
    module Foreign.Marshal.Array,
    module Data.Word,
    module Data.Bits,

  ) where


import Foreign.Storable
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.Word
import Data.Bits

#ifdef GRID_OPENGL_ES2
import OpenGL.ES2
import OpenGL.ES2.Values
import OpenGL.ES2.Types
#endif
#ifdef GRID_PLATFORM_IOS
import OpenGL.ES2.ExtIOS
#endif
#ifdef GRID_PLATFORM_GLFW
import OpenGL.ES2.ExtGLFW
#endif
