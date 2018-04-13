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
module OpenGL.ES2.ExtIOS where

import OpenGL.ES2.Types
import Foreign.Ptr
import Foreign.C.Types

-- GL_IMG_texture_compression_pvrtc
gl_COMPRESSED_RGB_PVRTC_4BPPV1_IMG :: GLenum
gl_COMPRESSED_RGB_PVRTC_4BPPV1_IMG = 0x8C00

gl_COMPRESSED_RGB_PVRTC_2BPPV1_IMG :: GLenum
gl_COMPRESSED_RGB_PVRTC_2BPPV1_IMG = 0x8C01

gl_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG :: GLenum
gl_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG = 0x8C02

gl_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG :: GLenum
gl_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG = 0x8C03

-- GL_OES_packed_depth_stencil
gl_DEPTH_STENCIL_OES :: GLenum
gl_DEPTH_STENCIL_OES = 0x84F9

gl_UNSIGNED_INT_24_8_OES :: GLenum
gl_UNSIGNED_INT_24_8_OES = 0x84FA

gl_DEPTH24_STENCIL8_OES :: GLenum 
gl_DEPTH24_STENCIL8_OES = 0x88F0

-- | GLvoid glDiscardFramebufferEXT(GLenum target, GLsizei numAttachments, const GLenum *attachments)
foreign import ccall unsafe "glDiscardFramebufferEXT" glDiscardFramebufferEXT
    :: GLenum -> GLsizei -> Ptr GLenum -> IO ()

-- | GLvoid glProgramUniform1iEXT(GLuint program, GLint location, GLint x)
foreign import ccall unsafe "glProgramUniform1iEXT" glProgramUniform1iEXT
    :: GLuint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform2iEXT(GLuint program, GLint location, GLint x, GLint y)  
foreign import ccall unsafe "glProgramUniform2iEXT" glProgramUniform2iEXT
    :: GLuint -> GLint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform3iEXT(GLuint program, GLint location, GLint x, GLint y, GLint z)  
foreign import ccall unsafe "glProgramUniform3iEXT" glProgramUniform3iEXT
    :: GLuint -> GLint -> GLint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform4iEXT(GLuint program, GLint location, GLint x, GLint y, GLint z, GLint w)
foreign import ccall unsafe "glProgramUniform4iEXT" glProgramUniform4iEXT
    :: GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> IO ()

-- | GLvoid glProgramUniform1fEXT(GLuint program, GLint location, GLfloat x)  
foreign import ccall unsafe "glProgramUniform1fEXT" glProgramUniform1fEXT
    :: GLuint -> GLint -> GLfloat -> IO ()

-- | GLvoid glProgramUniform2fEXT(GLuint program, GLint location, GLfloat x, GLfloat y)
foreign import ccall unsafe "glProgramUniform2fEXT" glProgramUniform2fEXT
    :: GLuint -> GLint -> GLfloat -> GLfloat -> IO ()

-- | GLvoid glProgramUniform3fEXT(GLuint program, GLint location, GLfloat x, GLfloat y, GLfloat z)  
foreign import ccall unsafe "glProgramUniform3fEXT" glProgramUniform3fEXT
    :: GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> IO ()

-- | GLvoid glProgramUniform4fEXT(GLuint program, GLint location, GLfloat x, GLfloat y, GLfloat z, GLfloat w)  
foreign import ccall unsafe "glProgramUniform4fEXT" glProgramUniform4fEXT
    :: GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

gl_WRITE_ONLY_OES :: GLenum
gl_WRITE_ONLY_OES = 0x88B9

gl_BUFFER_ACCESS_OES :: GLenum 
gl_BUFFER_ACCESS_OES = 0x88BB

gl_BUFFER_MAPPED_OES :: GLenum
gl_BUFFER_MAPPED_OES = 0x88BC

gl_BUFFER_MAP_POINTER_OES :: GLenum
gl_BUFFER_MAP_POINTER_OES = 0x88BD

-- | void glGetBufferPointervOES (GLenum target, GLenum pname, GLvoid **params);
foreign import ccall unsafe "glGetBufferPointervOES" glGetBufferPointervOES
    :: GLenum -> GLenum -> Ptr (Ptr a) -> IO ()

-- | GLvoid* glMapBufferOES (GLenum target, GLenum access);
foreign import ccall unsafe "glMapBufferOES" glMapBufferOES
    :: GLenum -> GLenum -> IO (Ptr GLvoid)

-- | GLboolean glUnmapBufferOES (GLenum target);
foreign import ccall unsafe "glUnmapBufferOES" glUnmapBufferOES
    :: GLenum -> IO GLboolean

-- | GLvoid glBindVertexArrayOES(GLuint array);  
foreign import ccall unsafe "glBindVertexArrayOES" glBindVertexArrayOES
    :: GLuint -> IO ()

-- | GLvoid glDeleteVertexArraysOES(GLsizei n, const GLuint *arrays);  
foreign import ccall unsafe "glDeleteVertexArraysOES" glDeleteVertexArraysOES
    :: GLsizei -> Ptr GLuint -> IO ()

-- | GLvoid glGenVertexArraysOES(GLsizei n, GLuint *arrays);  
foreign import ccall unsafe "glGenVertexArraysOES" glGenVertexArraysOES
    :: GLsizei -> Ptr GLuint -> IO ()

-- | GLboolean glIsVertexArrayOES(GLuint array);
foreign import ccall unsafe "glIsVertexArrayOES" glIsVertexArrayOES
    :: GLuint -> IO GLboolean


