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
module OpenGL.ES2.Types
  (
    GLvoid,
    GLchar,
    GLenum,
    GLboolean,
    GLbitfield,
    GLbyte,
    GLshort,
    GLint,
    GLsizei,
    GLubyte,
    GLushort,
    GLuint,
    GLfloat,
    GLclampf,
    GLfixed,
    GLclampx,
    GLintptr,
    GLsizeiptr,

  ) where

import Foreign.C.Types

type GLvoid =
    ()

type GLchar = 
    CChar

type GLenum =
    CUInt

type GLboolean =
    CUChar

type GLbitfield =
    CUInt

type GLbyte =
    CSChar

type GLshort =
    CShort

type GLint =
    CInt

type GLsizei =
    CInt

type GLubyte =
    CUChar

type GLushort =
    CUShort

type GLuint =
    CUInt

type GLfloat =
    CFloat

type GLclampf =
    CFloat

type GLfixed =
    CInt

type GLclampx =
    CInt

type GLintptr =
    CLong

type GLsizeiptr =
    CLong



