module OpenGL.IOS.Types
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



