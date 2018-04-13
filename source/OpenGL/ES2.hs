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
{-# LANGUAGE ForeignFunctionInterface #-}
module OpenGL.ES2 where
    
import OpenGL.ES2.Types
import Foreign.Ptr
import Foreign.C

-- | void         glActiveTexture (GLenum texture);
foreign import ccall unsafe "glActiveTexture" glActiveTexture
    :: GLenum -> IO ()

-- | void         glAttachShader (GLuint program, GLuint shader);
foreign import ccall unsafe "glAttachShader" glAttachShader
    :: GLuint -> GLuint -> IO ()

-- | void         glBindAttribLocation (GLuint program, GLuint index, const GLchar* name);  
foreign import ccall unsafe "glBindAttribLocation" glBindAttribLocation
    :: GLuint -> GLuint -> Ptr GLchar -> IO ()

-- | void         glBindBuffer (GLenum target, GLuint buffer);
foreign import ccall unsafe "glBindBuffer" glBindBuffer
    :: GLenum -> GLuint -> IO ()

-- | void         glBindTexture (GLenum target, GLuint texture);
foreign import ccall unsafe "glBindTexture" glBindTexture
    :: GLenum -> GLuint -> IO ()

-- | void         glBindFramebuffer (GLenum target, GLuint framebuffer);
foreign import ccall unsafe "glBindFramebuffer" glBindFramebuffer
    :: GLenum -> GLuint -> IO ()

-- | void         glBindRenderbuffer (GLenum target, GLuint renderbuffer) ; 
foreign import ccall unsafe "glBindRenderbuffer" glBindRenderbuffer
    :: GLenum -> GLuint -> IO ()

-- | void         glBlendColor (GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
foreign import ccall unsafe "glBlendColor" glBlendColor
    :: GLclampf -> GLclampf -> GLclampf -> GLclampf -> IO ()

-- | void         glBlendEquation ( GLenum mode );
foreign import ccall unsafe "glBlendEquation" glBlendEquation
    :: GLenum -> IO ()

-- | void         glBlendEquationSeparate (GLenum modeRGB, GLenum modeAlpha);
foreign import ccall unsafe "glBlendEquationSeparate" glBlendEquationSeparate
    :: GLenum -> GLenum -> IO ()

-- | void         glBlendFunc (GLenum sfactor, GLenum dfactor);
foreign import ccall unsafe "glBlendFunc" glBlendFunc
    :: GLenum -> GLenum -> IO ()
    
-- | void         glBlendFuncSeparate (GLenum srcRGB, GLenum dstRGB, GLenum srcAlpha, GLenum dstAlpha);
foreign import ccall unsafe "glBlendFuncSeparate" glBlendFuncSeparate
    :: GLenum -> GLenum -> GLenum -> GLenum -> IO ()

-- | void         glBufferData (GLenum target, GLsizeiptr size, const GLvoid* data, GLenum usage);
foreign import ccall unsafe "glBufferData" glBufferData
    :: GLenum -> GLsizeiptr -> Ptr a -> GLenum -> IO ()

-- | void         glBufferSubData (GLenum target, GLintptr offset, GLsizeiptr size, const GLvoid* data);
foreign import ccall unsafe "glBufferSubData" glBufferSubData
    :: GLenum -> GLintptr -> GLsizeiptr -> Ptr a -> IO ()

-- | GLenum       glCheckFramebufferStatus (GLenum target);
foreign import ccall unsafe "glCheckFramebufferStatus" glCheckFramebufferStatus
    :: GLenum -> IO GLenum

-- | void         glClear (GLbitfield mask);
foreign import ccall unsafe "glClear" glClear
    :: GLbitfield -> IO ()
    
-- | void         glClearColor (GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
foreign import ccall unsafe "glClearColor" glClearColor
    :: GLclampf -> GLclampf -> GLclampf -> GLclampf -> IO ()

-- | void         glClearDepthf (GLclampf depth);
foreign import ccall unsafe "glClearDepthf" glClearDepthf
    :: GLclampf -> IO ()

-- | void         glClearStencil (GLint s);
foreign import ccall unsafe "glClearStencil" glClearStencil
    :: GLint -> IO ()

-- | void         glColorMask (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
foreign import ccall unsafe "glColorMask" glColorMask
    :: GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ()

-- | void         glCompileShader (GLuint shader);
foreign import ccall unsafe "glCompileShader" glCompileShader
    :: GLuint -> IO ()

-- | void         glCopyTexImage2D (GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
foreign import ccall unsafe "glCopyTexImage2D" glCopyTexImage2D
    :: GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ()

-- | void         glCompressedTexImage2D (GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const GLvoid* data);
foreign import ccall unsafe "glCompressedTexImage2D" glCompressedTexImage2D
    :: GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ()

-- | void         glCompressedTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const GLvoid* data);
foreign import ccall unsafe "glCompressedTexSubImage2D" glCompressedTexSubImage2D
    :: GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ()

-- | void         glCopyTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
foreign import ccall unsafe "glCopyTexSubImage2D" glCopyTexSubImage2D
    :: GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()
     
-- | GLuint       glCreateProgram (void);
foreign import ccall unsafe "glCreateProgram" glCreateProgram
    :: IO GLuint

-- | GLuint       glCreateShader (GLenum type);
foreign import ccall unsafe "glCreateShader" glCreateShader
    :: GLenum -> IO GLuint

-- | void         glCullFace (GLenum mode);
foreign import ccall unsafe "glCullFace" glCullFace
    :: GLenum -> IO ()

-- | void         glDeleteBuffers (GLsizei n, const GLuint* buffers);
foreign import ccall unsafe "glDeleteBuffers" glDeleteBuffers
    :: GLsizei -> Ptr GLuint -> IO ()

-- | void         glDeleteFramebuffers (GLsizei n, const GLuint* framebuffers);
foreign import ccall unsafe "glDeleteFramebuffers" glDeleteFramebuffers
    :: GLsizei -> Ptr GLuint -> IO ()

-- | void         glDeleteProgram (GLuint program);
foreign import ccall unsafe "glDeleteProgram" glDeleteProgram
    :: GLuint -> IO ()

-- | void         glDeleteRenderbuffers (GLsizei n, const GLuint* renderbuffers);
foreign import ccall unsafe "glDeleteRenderbuffers" glDeleteRenderbuffers
    :: GLsizei -> Ptr GLuint -> IO ()

-- | void         glDeleteShader (GLuint shader);
foreign import ccall unsafe "glDeleteShader" glDeleteShader
    :: GLuint -> IO ()

-- | void         glDeleteTextures (GLsizei n, const GLuint* textures);
foreign import ccall unsafe "glDeleteTextures" glDeleteTextures
    :: GLsizei -> Ptr GLuint -> IO ()

-- | void         glDepthFunc (GLenum func);
foreign import ccall unsafe "glDepthFunc" glDepthFunc
    :: GLenum -> IO ()

-- | void         glDepthMask (GLboolean flag);
foreign import ccall unsafe "glDepthMask" glDepthMask
    :: GLboolean -> IO ()

-- | void         glDepthRangef (GLclampf zNear, GLclampf zFar);
foreign import ccall unsafe "glDepthRangef" glDepthRangef
    :: GLclampf -> GLclampf -> IO ()

-- | void         glDetachShader (GLuint program, GLuint shader);
foreign import ccall unsafe "glDetachShader" glDetachShader
    :: GLuint -> GLuint -> IO ()

-- | void         glDisable (GLenum cap);
foreign import ccall unsafe "glDisable" glDisable
    :: GLenum -> IO ()

-- | void         glDisableVertexAttribArray (GLuint index);
foreign import ccall unsafe "glDisableVertexAttribArray" glDisableVertexAttribArray
    :: GLuint -> IO ()

-- | void         glDrawArrays (GLenum mode, GLint first, GLsizei count);
foreign import ccall unsafe "glDrawArrays" glDrawArrays
    :: GLenum -> GLint -> GLsizei -> IO ()

-- | void         glDrawElements (GLenum mode, GLsizei count, GLenum type, const GLvoid* indices);
foreign import ccall unsafe "glDrawElements" glDrawElements
    :: GLenum -> GLsizei -> GLenum -> Ptr a -> IO ()

-- | void         glEnable (GLenum cap);
foreign import ccall unsafe "glEnable" glEnable
    :: GLenum -> IO ()

-- | void         glEnableVertexAttribArray (GLuint index);
foreign import ccall unsafe "glEnableVertexAttribArray" glEnableVertexAttribArray
    :: GLuint -> IO ()

-- | void         glFinish (void);
foreign import ccall unsafe "glFinish" glFinish
    :: IO ()

-- | void         glFlush (void);
foreign import ccall unsafe "glFlush" glFlush
    :: IO ()

-- | void         glFramebufferRenderbuffer (GLenum target, GLenum attachment, GLenum renderbuffertarget, GLuint renderbuffer);
foreign import ccall unsafe "glFramebufferRenderbuffer" glFramebufferRenderbuffer
    :: GLenum -> GLenum -> GLenum -> GLuint -> IO ()

-- | void         glFramebufferTexture2D (GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level);
foreign import ccall unsafe "glFramebufferTexture2D" glFramebufferTexture2D
    :: GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ()

-- | void         glFrontFace (GLenum mode);
foreign import ccall unsafe "glFrontFace" glFrontFace
    :: GLenum -> IO ()

-- | void         glGenBuffers (GLsizei n, GLuint* buffers);
foreign import ccall unsafe "glGenBuffers" glGenBuffers
    :: GLsizei -> Ptr GLuint -> IO ()

-- | void         glGenerateMipmap (GLenum target);
foreign import ccall unsafe "glGenerateMipmap" glGenerateMipmap
    :: GLenum -> IO ()

-- | void         glGenFramebuffers (GLsizei n, GLuint* framebuffers);
foreign import ccall unsafe "glGenFramebuffers" glGenFramebuffers
    :: GLsizei -> Ptr GLuint -> IO ()

-- | void         glGenRenderbuffers (GLsizei n, GLuint* renderbuffers);
foreign import ccall unsafe "glGenRenderbuffers" glGenRenderbuffers
    :: GLsizei -> Ptr GLuint -> IO ()

-- | void         glGenTextures (GLsizei n, GLuint* textures);
foreign import ccall unsafe "glGenTextures" glGenTextures
    :: GLsizei -> Ptr GLuint -> IO ()

-- | void         glGetActiveAttrib (GLuint program, GLuint index, GLsizei bufsize, GLsizei* length, GLint* size, GLenum* type, GLchar* name);
foreign import ccall unsafe "glGetActiveAttrib" glGetActiveAttrib
    :: GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ()

-- | void         glGetActiveUniform (GLuint program, GLuint index, GLsizei bufsize, GLsizei* length, GLint* size, GLenum* type, GLchar* name);
foreign import ccall unsafe "glGetActiveUniform" glGetActiveUniform
    :: GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ()

-- | void         glGetAttachedShaders (GLuint program, GLsizei maxcount, GLsizei* count, GLuint* shaders);
foreign import ccall unsafe "glGetAttachedShaders" glGetAttachedShaders
    :: GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> IO ()

-- | int          glGetAttribLocation (GLuint program, const GLchar* name); NOTE: int instead of GLint is probably a bug. assuming GLint!
foreign import ccall unsafe "glGetAttribLocation" glGetAttribLocation
    :: GLuint -> Ptr GLchar -> IO GLint

-- | void         glGetBooleanv (GLenum pname, GLboolean* params);
foreign import ccall unsafe "glGetBooleanv" glGetBooleanv
    :: GLenum -> Ptr GLboolean -> IO ()

-- | void         glGetBufferParameteriv (GLenum target, GLenum pname, GLint* params);
foreign import ccall unsafe "glGetBufferParameteriv" glGetBufferParameteriv
    :: GLenum -> GLenum -> Ptr GLint -> IO ()

-- | GLenum       glGetError (void);
foreign import ccall unsafe "glGetError" glGetError
    :: IO GLenum
    
-- | void         glGetFloatv (GLenum pname, GLfloat* params);
foreign import ccall unsafe "glGetFloatv" glGetFloatv
    :: GLenum -> GLfloat -> IO ()

-- | void         glGetFramebufferAttachmentParameteriv (GLenum target, GLenum attachment, GLenum pname, GLint* params);
foreign import ccall unsafe "glGetFramebufferAttachmentParameteriv" glGetFramebufferAttachmentParameteriv
    :: GLenum -> GLenum -> Ptr GLint -> IO ()

-- | void         glGetIntegerv (GLenum pname, GLint* params);
foreign import ccall unsafe "glGetIntegerv" glGetIntegerv
    :: GLenum -> Ptr GLint -> IO ()

-- | void         glGetProgramiv (GLuint program, GLenum pname, GLint* params);
foreign import ccall unsafe "glGetProgramiv" glGetProgramiv
    :: GLuint -> GLenum -> Ptr GLint -> IO ()

-- | void         glGetProgramInfoLog (GLuint program, GLsizei bufsize, GLsizei* length, GLchar* infolog);
foreign import ccall unsafe "glGetProgramInfoLog" glProgramInfoLog
    :: GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()

-- | void         glGetRenderbufferParameteriv (GLenum target, GLenum pname, GLint* params);
foreign import ccall unsafe "glGetRenderbufferParameteriv" glGetRenderbufferParameteriv
    :: GLenum -> GLenum -> Ptr GLint -> IO ()

-- | void         glGetShaderiv (GLuint shader, GLenum pname, GLint* params);
foreign import ccall unsafe "glGetShaderiv" glGetShaderiv
    :: GLuint -> GLenum -> Ptr GLint -> IO ()

-- | void         glGetShaderInfoLog (GLuint shader, GLsizei bufsize, GLsizei* length, GLchar* infolog);
foreign import ccall unsafe "glGetShaderInfoLog" glGetShaderInfoLog
    :: GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()

-- | void         glGetShaderPrecisionFormat (GLenum shadertype, GLenum precisiontype, GLint* range, GLint* precision);
foreign import ccall unsafe "glGetShaderPrecisionFormat" glGetShaderPrecisionFormat 
    :: GLenum -> GLenum -> Ptr GLint -> Ptr GLint -> IO ()

-- | void         glGetShaderSource (GLuint shader, GLsizei bufsize, GLsizei* length, GLchar* source);
foreign import ccall unsafe "glGetShaderSource" glGetShaderSource
    :: GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()

-- | const GLubyte* glGetString (GLenum name);
foreign import ccall unsafe "glGetString" glGetString
    :: GLenum -> IO (Ptr GLubyte)

-- | void         glGetTexParameterfv (GLenum target, GLenum pname, GLfloat* params);
foreign import ccall unsafe "glGetTexParameterfv" glGetTexParameterfv
    :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

-- | void         glGetTexParameteriv (GLenum target, GLenum pname, GLint* params);
foreign import ccall unsafe "glGetTexParameteriv" glGetTexParameteriv
    :: GLenum -> GLenum -> Ptr GLint -> IO ()

-- | void         glGetUniformfv (GLuint program, GLint location, GLfloat* params);
foreign import ccall unsafe "glGetUniformfv" glGetUniformfv 
    :: GLuint -> GLint -> Ptr GLfloat -> IO ()

-- | void         glGetUniformiv (GLuint program, GLint location, GLint* params);
foreign import ccall unsafe "glGetUniformiv" glGetUniformiv
    :: GLuint -> GLint -> Ptr GLint -> IO ()

-- | int          glGetUniformLocation (GLuint program, const GLchar* name); NOTE: int instead of GLint is proably a bug. assuming GLint!
foreign import ccall unsafe "glGetUniformLocation" glGetUniformLocation
    :: GLuint -> Ptr GLchar -> IO GLint

-- | void         glGetVertexAttribfv (GLuint index, GLenum pname, GLfloat* params);
foreign import ccall unsafe "glGetVertexAttribfv" glGetVertexAttribfv
    :: GLuint -> GLenum -> Ptr GLfloat -> IO ()

-- | void         glGetVertexAttribiv (GLuint index, GLenum pname, GLint* params);
foreign import ccall unsafe "glGetVertexAttribiv" glGetVertexAttribiv
    :: GLuint -> GLenum -> Ptr GLint -> IO ()

-- | void         glGetVertexAttribPointerv (GLuint index, GLenum pname, GLvoid** pointer);
foreign import ccall unsafe "glGetVertexAttribPointerv" glGetVertexAttribPointerv
    :: GLuint -> GLenum -> Ptr (Ptr a) -> IO ()

-- | void         glHint (GLenum target, GLenum mode);
foreign import ccall unsafe "glHint" glHint
    :: GLenum -> GLenum -> IO ()

-- | GLboolean    glIsBuffer (GLuint buffer);
foreign import ccall unsafe "glIsBuffer" glIsBuffer
    :: GLuint -> IO GLboolean

-- | GLboolean    glIsEnabled (GLenum cap);
foreign import ccall unsafe "glIsEnabled" glIsEnabled
    :: GLenum -> IO GLboolean

-- | GLboolean    glIsFramebuffer (GLuint framebuffer);
foreign import ccall unsafe "glIsFramebuffer" glIsFramebuffer
    :: GLuint -> IO GLboolean

-- | GLboolean    glIsProgram (GLuint program);
foreign import ccall unsafe "glIsProgram" glIsProgram
    :: GLuint -> IO GLboolean

-- | GLboolean    glIsRenderbuffer (GLuint renderbuffer);
foreign import ccall unsafe "glIsRenderbuffer" glIsRenderbuffer
    :: GLuint -> IO GLboolean

-- | GLboolean    glIsShader (GLuint shader);
foreign import ccall unsafe "glIsShader" glIsShader
    :: GLuint -> IO GLboolean

-- | GLboolean    glIsTexture (GLuint texture);
foreign import ccall unsafe "glIsTexture" glIsTexture
    :: GLuint -> IO GLboolean

-- | void         glLineWidth (GLfloat width);
foreign import ccall unsafe "glLineWidth" glLineWidth
    :: GLfloat -> IO ()

-- | void         glLinkProgram (GLuint program);
foreign import ccall unsafe "glLinkProgram" glLinkProgram
    :: GLuint -> IO ()

-- | void         glPixelStorei (GLenum pname, GLint param);
foreign import ccall unsafe "glPixelStorei" glPixelStorei
    :: GLenum -> GLint -> IO ()

-- | void         glPolygonOffset (GLfloat factor, GLfloat units);
foreign import ccall unsafe "glPolygonOffset" glPolygonOffset
    :: GLfloat -> GLfloat -> IO ()

-- | void         glReadPixels (GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid* pixels);
foreign import ccall unsafe "glReadPixels" glReadPixels
    :: GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ()

-- | void         glReleaseShaderCompiler (void);
foreign import ccall unsafe "glReleaseShaderCompiler" glReleaseShaderCompiler
    :: IO ()

-- | void         glRenderbufferStorage (GLenum target, GLenum internalformat, GLsizei width, GLsizei height);
foreign import ccall unsafe "glRenderbufferStorage" glRenderbufferStorage
    :: GLenum -> GLenum -> GLsizei -> GLsizei -> IO ()

-- | void         glSampleCoverage (GLclampf value, GLboolean invert);
foreign import ccall unsafe "glSampleCoverage" glSampleCoverage
    :: GLclampf -> GLboolean -> IO ()

-- | void         glScissor (GLint x, GLint y, GLsizei width, GLsizei height);
foreign import ccall unsafe "glScissor" glScissor
    :: GLint -> GLint -> GLsizei -> GLsizei -> IO ()

-- | void         glShaderBinary (GLsizei n, const GLuint* shaders, GLenum binaryformat, const GLvoid* binary, GLsizei length);
foreign import ccall unsafe "glShaderBinary" glShaderBinary
    :: GLsizei -> Ptr GLuint -> GLenum -> Ptr a -> GLsizei -> IO ()

-- | void         glShaderSource (GLuint shader, GLsizei count, const GLchar** string, const GLint* length);
foreign import ccall unsafe "glShaderSource" glShaderSource
    :: GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ()

-- | void         glStencilFunc (GLenum func, GLint ref, GLuint mask);
foreign import ccall unsafe "glStencilFunc" glStencilFunc
    :: GLenum -> GLint -> GLuint -> IO ()

-- | void         glStencilFuncSeparate (GLenum face, GLenum func, GLint ref, GLuint mask);
foreign import ccall unsafe "glStencilFuncSeparate" glStencilFuncSeparate
    :: GLenum -> GLenum -> GLint -> GLuint -> IO ()

-- | void         glStencilMask (GLuint mask);
foreign import ccall unsafe "glStencilMask" glStencilMask
    :: GLuint -> IO ()

-- | void         glStencilMaskSeparate (GLenum face, GLuint mask);
foreign import ccall unsafe "glStencilMaskSeparate" glStencilMaskSeparate
    :: GLenum -> GLuint -> IO ()

-- | void         glStencilOp (GLenum fail, GLenum zfail, GLenum zpass);
foreign import ccall unsafe "glStencilOp" glStencilOp
    :: GLenum -> GLenum -> GLenum -> IO ()

-- | void         glStencilOpSeparate (GLenum face, GLenum fail, GLenum zfail, GLenum zpass);
foreign import ccall unsafe "glStencilOpSeparate" glStencilOpSeparate
    :: GLenum -> GLenum -> GLenum -> GLenum -> IO ()

-- | void         glTexImage2D (GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const GLvoid* pixels);
foreign import ccall unsafe "glTexImage2D" glTexImage2D
    :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ()

-- | void         glTexParameterf (GLenum target, GLenum pname, GLfloat param);
foreign import ccall unsafe "glTexParameterf" glTexParameterf
    :: GLenum -> GLenum -> GLfloat -> IO ()

-- | void         glTexParameterfv (GLenum target, GLenum pname, const GLfloat* params);
foreign import ccall unsafe "glTexParameterfv" glTexParameterfv
    :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

-- | void         glTexParameteri (GLenum target, GLenum pname, GLint param);
foreign import ccall unsafe "glTexParameteri" glTexParameteri
    :: GLenum -> GLenum -> GLint -> IO ()

-- | void         glTexParameteriv (GLenum target, GLenum pname, const GLint* params);
foreign import ccall unsafe "glTexParameteriv" glTexParameteriv
    :: GLenum -> GLenum -> Ptr GLint -> IO ()

-- | void         glTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid* pixels);
foreign import ccall unsafe "glTexSubImage2D" glTexSubImage2D
    :: GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ()

-- | void         glUniform1f (GLint location, GLfloat x);
foreign import ccall unsafe "glUniform1f" glUniform1f
    :: GLint -> GLfloat -> IO ()

-- | void         glUniform1fv (GLint location, GLsizei count, const GLfloat* v);
foreign import ccall unsafe "glUniform1fv" glUniform1fv
    :: GLint -> GLsizei -> Ptr GLfloat -> IO ()

-- | void         glUniform1i (GLint location, GLint x);
foreign import ccall unsafe "glUniform1i" glUniform1i
    :: GLint -> GLint -> IO ()

-- | void         glUniform1iv (GLint location, GLsizei count, const GLint* v);
foreign import ccall unsafe "glUniform1iv" glUniform1iv
    :: GLint -> GLsizei -> Ptr GLint -> IO ()

-- | void         glUniform2f (GLint location, GLfloat x, GLfloat y);
foreign import ccall unsafe "glUniform2f" glUniform2f
    :: GLint -> GLfloat -> GLfloat -> IO ()

-- | void         glUniform2fv (GLint location, GLsizei count, const GLfloat* v);
foreign import ccall unsafe "glUniform2fv" glUniform2fv
    :: GLint -> GLsizei -> Ptr GLfloat -> IO ()

-- | void         glUniform2i (GLint location, GLint x, GLint y);
foreign import ccall unsafe "glUniform2i" glUniform2i
    :: GLint -> GLint -> GLint -> IO ()

-- | void         glUniform2iv (GLint location, GLsizei count, const GLint* v);
foreign import ccall unsafe "glUniform2iv" glUniform2iv
    :: GLint -> GLsizei -> Ptr GLint -> IO ()

-- | void         glUniform3f (GLint location, GLfloat x, GLfloat y, GLfloat z);
foreign import ccall unsafe "glUniform3f" glUniform3f
    :: GLint -> GLfloat -> GLfloat -> GLfloat -> IO ()

-- | void         glUniform3fv (GLint location, GLsizei count, const GLfloat* v);
foreign import ccall unsafe "glUniform3fv" glUniform3fv
    :: GLint -> GLsizei -> Ptr GLfloat -> IO ()

-- | void         glUniform3i (GLint location, GLint x, GLint y, GLint z);
foreign import ccall unsafe "glUniform3i" glUniform3i
    :: GLint -> GLint -> GLint -> GLint -> IO ()

-- | void         glUniform3iv (GLint location, GLsizei count, const GLint* v);
foreign import ccall unsafe "glUniform3iv" glUniform3iv
    :: GLint -> GLsizei -> Ptr GLint -> IO ()
    
-- | void         glUniform4f (GLint location, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
foreign import ccall unsafe "glUniform4f" glUniform4f
    :: GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

-- | void         glUniform4fv (GLint location, GLsizei count, const GLfloat* v);
foreign import ccall unsafe "glUniform4fv" glUniform4fv
    :: GLint -> GLsizei -> Ptr GLfloat -> IO ()

-- | void         glUniform4i (GLint location, GLint x, GLint y, GLint z, GLint w);
foreign import ccall unsafe "glUniform4i" glUniform4i
    :: GLint -> GLint -> GLint -> GLint -> GLint -> IO ()

-- | void         glUniform4iv (GLint location, GLsizei count, const GLint* v);
foreign import ccall unsafe "glUniform4iv" glUniform4iv
    :: GLint -> GLsizei -> Ptr GLint -> IO ()

-- | void         glUniformMatrix2fv (GLint location, GLsizei count, GLboolean transpose, const GLfloat* value);
foreign import ccall unsafe "glUniformMatrix2fv" glUnformMatrix2fv
    :: GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ()

-- | void         glUniformMatrix3fv (GLint location, GLsizei count, GLboolean transpose, const GLfloat* value);
foreign import ccall unsafe "glUniformMatrix3fv" glUniformMatrix3fv
    :: GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ()

-- | void         glUniformMatrix4fv (GLint location, GLsizei count, GLboolean transpose, const GLfloat* value);
foreign import ccall unsafe "glUniformMatrix4fv" glUniformMatrix4fv
    :: GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ()

-- | void         glUseProgram (GLuint program);
foreign import ccall unsafe "glUseProgram" glUseProgram
    :: GLuint -> IO ()

-- | void         glValidateProgram (GLuint program);
foreign import ccall unsafe "glValidateProgram" glValidateProgram
    :: GLuint -> IO ()

-- | void         glVertexAttrib1f (GLuint indx, GLfloat x);
foreign import ccall unsafe "glVertexAttrib1f" glVertexAttrib1f
    :: GLuint -> GLfloat -> IO ()

-- | void         glVertexAttrib1fv (GLuint indx, const GLfloat* values);
foreign import ccall unsafe "glVertexAttrib1fv" glVertexAttrib1fv
    :: GLuint -> Ptr GLfloat -> IO ()

-- | void         glVertexAttrib2f (GLuint indx, GLfloat x, GLfloat y);
foreign import ccall unsafe "glVertexAttrib2f" glVertexAttrib2f
    :: GLuint -> GLfloat -> GLfloat -> IO ()

-- | void         glVertexAttrib2fv (GLuint indx, const GLfloat* values);
foreign import ccall unsafe "glVertexAttrib2fv" glVertexAttrib2fv
    :: GLuint -> Ptr GLfloat -> IO ()

-- | void         glVertexAttrib3f (GLuint indx, GLfloat x, GLfloat y, GLfloat z);
foreign import ccall unsafe "glVertexAttrib3f" glVertexAttrib3f
    :: GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ()

-- | void         glVertexAttrib3fv (GLuint indx, const GLfloat* values);
foreign import ccall unsafe "glVertexAttrib3fv" glVertexAttrib3fv
    :: GLuint -> Ptr GLfloat -> IO ()

-- | void         glVertexAttrib4f (GLuint indx, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
foreign import ccall unsafe "glVertexAttrib4f" glVertexAttrib4f
    :: GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

-- | void         glVertexAttrib4fv (GLuint indx, const GLfloat* values);
foreign import ccall unsafe "glVertexAttrib4fv" glVertexAttrib4fv
    :: GLuint -> Ptr GLfloat -> IO ()

-- | void         glVertexAttribPointer (GLuint indx, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const GLvoid* ptr);
foreign import ccall unsafe "glVertexAttribPointer" glVertexAttribPointer
    :: GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr a -> IO ()

-- | void         glViewport (GLint x, GLint y, GLsizei width, GLsizei height);
foreign import ccall unsafe "glViewport" glViewport
    :: GLint -> GLint -> GLsizei -> GLsizei -> IO ()

