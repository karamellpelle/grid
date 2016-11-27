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
module OpenGL.ES2.ExtGLFW where

import OpenGL.ES2.Types
import Foreign.Ptr
import Foreign.C.Types



--------------------------------------------------------------------------------
--  NOTE
--  
--  this module tries to emulate iOS GL extensions by using the similar functions
--  without extension naming. it is unsure wheter this works as intented. rewriting
--  code should insted be done!
--


--------------------------------------------------------------------------------
--  GL_IMG_texture_compression_pvrtc
--
--  FIXME: write other in code!!

gl_COMPRESSED_RGB_PVRTC_4BPPV1_IMG :: GLenum
gl_COMPRESSED_RGB_PVRTC_4BPPV1_IMG = 0x8C00

gl_COMPRESSED_RGB_PVRTC_2BPPV1_IMG :: GLenum
gl_COMPRESSED_RGB_PVRTC_2BPPV1_IMG = 0x8C01

gl_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG :: GLenum
gl_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG = 0x8C02

gl_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG :: GLenum
gl_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG = 0x8C03


--------------------------------------------------------------------------------
--  GL_ARB_framebuffer_object
--

-- GL_OES_packed_depth_stencil
gl_DEPTH_STENCIL_OES :: GLenum
gl_DEPTH_STENCIL_OES = 0x84F9

gl_UNSIGNED_INT_24_8_OES :: GLenum
gl_UNSIGNED_INT_24_8_OES = 0x84FA

gl_DEPTH24_STENCIL8_OES :: GLenum 
gl_DEPTH24_STENCIL8_OES = 0x88F0


--------------------------------------------------------------------------------
-- GLEW_ARB_separate_shader_objects 
--
-- TODO: verify "EXT" -> ""

-- | GLvoid glProgramUniform1iEXT(GLuint program, GLint location, GLint x)
foreign import ccall unsafe "glProgramUniform1i" glProgramUniform1iEXT
    :: GLuint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform2iEXT(GLuint program, GLint location, GLint x, GLint y)  
foreign import ccall unsafe "glProgramUniform2i" glProgramUniform2iEXT
    :: GLuint -> GLint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform3iEXT(GLuint program, GLint location, GLint x, GLint y, GLint z)  
foreign import ccall unsafe "glProgramUniform3i" glProgramUniform3iEXT
    :: GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform4iEXT(GLuint program, GLint location, GLint x, GLint y, GLint z, GLint w)
foreign import ccall unsafe "glProgramUniform4i" glProgramUniform4iEXT
    :: GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform1fEXT(GLuint program, GLint location, GLfloat x)  
foreign import ccall unsafe "glProgramUniform1f" glProgramUniform1fEXT
    :: GLuint -> GLint -> GLfloat -> IO ()

-- | GLvoid glProgramUniform2fEXT(GLuint program, GLint location, GLfloat x, GLfloat y)
foreign import ccall unsafe "glProgramUniform2f" glProgramUniform2fEXT
    :: GLuint -> GLint -> GLfloat -> GLfloat -> IO ()

-- | GLvoid glProgramUniform3fEXT(GLuint program, GLint location, GLfloat x, GLfloat y, GLfloat z)  
foreign import ccall unsafe "glProgramUniform3f" glProgramUniform3fEXT
    :: GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> IO ()

-- | GLvoid glProgramUniform4fEXT(GLuint program, GLint location, GLfloat x, GLfloat y, GLfloat z, GLfloat w)  
foreign import ccall unsafe "glProgramUniform4f" glProgramUniform4fEXT
    :: GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()


--------------------------------------------------------------------------------
--  GL_OES_vertex_array_object
--
--  TODO: verify "OES" -> ""

gl_WRITE_ONLY_OES :: GLenum
gl_WRITE_ONLY_OES = 0x88B9

gl_BUFFER_ACCESS_OES :: GLenum 
gl_BUFFER_ACCESS_OES = 0x88BB

gl_BUFFER_MAPPED_OES :: GLenum
gl_BUFFER_MAPPED_OES = 0x88BC

gl_BUFFER_MAP_POINTER_OES :: GLenum
gl_BUFFER_MAP_POINTER_OES = 0x88BD


-- | void glGetBufferPointervOES (GLenum target, GLenum pname, GLvoid **params);
foreign import ccall unsafe "glGetBufferPointerv" glGetBufferPointervOES
    :: GLenum -> GLenum -> Ptr (Ptr a) -> IO ()

-- | GLvoid* glMapBufferOES (GLenum target, GLenum access);
foreign import ccall unsafe "glMapBuffer" glMapBufferOES
    :: GLenum -> GLenum -> IO (Ptr GLvoid)

-- | GLboolean glUnmapBufferOES (GLenum target);
foreign import ccall unsafe "glUnmapBuffer" glUnmapBufferOES
    :: GLenum -> IO GLboolean

-- | GLvoid glBindVertexArrayOES(GLuint array);  
foreign import ccall unsafe "glBindVertexArray" glBindVertexArrayOES
    :: GLuint -> IO ()

-- | GLvoid glDeleteVertexArraysOES(GLsizei n, const GLuint *arrays);  
foreign import ccall unsafe "glDeleteVertexArrays" glDeleteVertexArraysOES
    :: GLsizei -> Ptr GLuint -> IO ()

-- | GLvoid glGenVertexArraysOES(GLsizei n, GLuint *arrays);  
foreign import ccall unsafe "glGenVertexArrays" glGenVertexArraysOES
    :: GLsizei -> Ptr GLuint -> IO ()

-- | GLboolean glIsVertexArrayOES(GLuint array);
foreign import ccall unsafe "glIsVertexArray" glIsVertexArrayOES
    :: GLuint -> IO GLboolean


