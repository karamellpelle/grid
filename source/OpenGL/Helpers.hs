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
{-# LANGUAGE ForeignFunctionInterface #-}
module OpenGL.Helpers
  (
    debugGLError,
    normGLushort,
    mkPtrGLvoid,
    
    makeGroupIBO,

    uniformMat4,
    uniformMat4AsMat3,
    getUniformLocation,
    uniformLocations,

    bindNewBuf,
    bindNewFBO,
    bindNewRBO,
    bindNewVAO,
    bindNewTex,
    delBuf,
    delFBO,
    delRBO,
    delVAO,
    delTex,

    writeBuf,

    createPrg,
    createPPO,
    createShader,

    loadTexPreMult,

    discardFramebuffer,

    module Linear,
  ) where



import MyPrelude
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import Control.Exception as C

import OpenGL
import Linear



debugGLError :: String -> IO ()
debugGLError tag = do
    err <- glGetError
    case err of
        0x0000  -> return ()
        0x0500  -> putStrLn $ tag ++ " GL_INVALID_ENUM"
        0x0501  -> putStrLn $ tag ++ " GL_INVALID_VALUE"
        0x0502  -> putStrLn $ tag ++ " GL_INVALID_OPERATION"
        0x0506  -> putStrLn $ tag ++ " GL_INVALID_FRAMEBUFFER_OPERATION"
        0x0505  -> putStrLn $ tag ++ " GL_OUT_OF_MEMORY"
        err     -> putStrLn $ tag ++ " " ++ show err


mkPtrGLvoid :: UInt -> Ptr GLvoid
mkPtrGLvoid n = 
    plusPtr nullPtr (fI n)



normGLushort :: Float -> GLushort
normGLushort alpha = 
    truncate $ fI (complement 0 :: GLushort) * alpha


--------------------------------------------------------------------------------
--  index buffer object

-- | create ibo for 'm' disjoint groups of size 'size'.
--   bytesize is: m * (size + 2) * 2
--  (gl_TRIANGLE_STRIP, gl_UNSIGNED_SHORT, max different indices: 2^16)
makeGroupIBO :: UInt -> UInt -> IO GLuint
makeGroupIBO size m = do
    ibo <- bindNewBuf gl_ELEMENT_ARRAY_BUFFER
    let bytesize = (m * (size + 2)) * 2
    withArray (helper 0) $ \ptr ->
        glBufferData gl_ELEMENT_ARRAY_BUFFER (fI bytesize) ptr gl_STATIC_DRAW
    return ibo

    where
      helper :: UInt -> [GLushort]
      helper n 
          | n == m      = []
          -- prevent index out of bounds on last element:
          | n + 1 == m  = let n' = n + 1
                          in  range (fI $ n * size) (fI $ n' * size) ++
                              [fI $ n' * size - 1, fI $ n' * size - 1]
          | otherwise   = let n' = n + 1
                          in  range (fI $ n * size) (fI $ n' * size) ++ 
                              [fI $ n' * size - 1, fI $ n' * size] ++
                              helper (n + 1)
      range i j = 
          if i == j then [] else i : range (i + 1) j




--------------------------------------------------------------------------------
--  

uniformMat4 :: GLint -> Mat4 -> IO ()
uniformMat4 loc (Mat4 x0 x1 x2 x3
                      y0 y1 y2 y3
                      z0 z1 z2 z3
                      w0 w1 w2 w3) =
    allocaArray 16 $ \ptr -> do
        pokeElemOff ptr 0   $ realToFrac x0
        pokeElemOff ptr 1   $ realToFrac x1
        pokeElemOff ptr 2   $ realToFrac x2
        pokeElemOff ptr 3   $ realToFrac x3
        pokeElemOff ptr 4   $ realToFrac y0
        pokeElemOff ptr 5   $ realToFrac y1
        pokeElemOff ptr 6   $ realToFrac y2
        pokeElemOff ptr 7   $ realToFrac y3
        pokeElemOff ptr 8   $ realToFrac z0
        pokeElemOff ptr 9   $ realToFrac z1
        pokeElemOff ptr 10  $ realToFrac z2
        pokeElemOff ptr 11  $ realToFrac z3
        pokeElemOff ptr 12  $ realToFrac w0
        pokeElemOff ptr 13  $ realToFrac w1
        pokeElemOff ptr 14  $ realToFrac w2
        pokeElemOff ptr 15  $ realToFrac w3
        
        glUniformMatrix4fv loc 1 gl_FALSE ptr


-- | use 3x3 part of 4x4
uniformMat4AsMat3 :: GLint -> Mat4 -> IO ()
uniformMat4AsMat3 loc (Mat4 x0 x1 x2 x3
                            y0 y1 y2 y3
                            z0 z1 z2 z3
                            w0 w1 w2 w3) =
    allocaArray 9 $ \ptr -> do
        pokeElemOff ptr 0   $ realToFrac x0
        pokeElemOff ptr 1   $ realToFrac x1
        pokeElemOff ptr 2   $ realToFrac x2
        pokeElemOff ptr 3   $ realToFrac y0
        pokeElemOff ptr 4   $ realToFrac y1
        pokeElemOff ptr 5   $ realToFrac y2
        pokeElemOff ptr 6   $ realToFrac z0
        pokeElemOff ptr 7   $ realToFrac z1
        pokeElemOff ptr 8   $ realToFrac z2
        
        glUniformMatrix3fv loc 1 gl_FALSE ptr


-- | fixme: add (tag :: String) as first parameter, for better warnings
getUniformLocation :: GLuint -> String -> IO GLint
getUniformLocation prg name = 
    withCString name $ \cstr -> do
        glGetUniformLocation prg cstr >>= \loc -> case loc of
            (-1)    -> do
#ifdef DEBUG
                putStrLn $ "no such uniform location: " ++ name
#endif
                return loc

            loc     -> 
                return loc

uniformLocations :: GLuint -> [String] -> IO [GLint]
uniformLocations prg names =
    case names of
        []            -> 
            return []

        (name:names)  -> do
            withCString name $ \cstr -> do
                glGetUniformLocation prg cstr >>= \loc -> case loc of
                    (-1)    -> do
#ifdef DEBUG
                        putStrLn $ "no such uniform location: " ++ name
#endif
                        uniformLocations prg names >>= \locs -> return (loc:locs)
                    loc     -> 
                        uniformLocations prg names >>= \locs -> 
                            return (loc:locs)

--------------------------------------------------------------------------------
--  bind new

bindNewBuf :: GLenum -> IO GLuint
bindNewBuf target = do
    alloca $ \ptr -> do
        glGenBuffers 1 ptr
        buf <- peek ptr
        glBindBuffer target buf
        return buf

bindNewFBO :: IO GLuint
bindNewFBO = 
    alloca $ \ptr -> do
        glGenFramebuffers 1 ptr
        fbo <- peek ptr
        glBindFramebuffer gl_FRAMEBUFFER fbo
        return fbo


bindNewRBO :: IO GLuint
bindNewRBO = 
    alloca $ \ptr -> do
        glGenRenderbuffers 1 ptr
        rbo <- peek ptr
        glBindRenderbuffer gl_RENDERBUFFER rbo
        return rbo


bindNewTex :: GLenum -> IO GLuint
bindNewTex target = 
    alloca $ \ptr -> do
        glGenTextures 1 ptr
        tex <- peek ptr
        glBindTexture target tex
        return tex


bindNewVAO :: IO GLuint
bindNewVAO =
    alloca $ \ptr -> do
        glGenVertexArraysOES 1 ptr
        vao <- peek ptr
        glBindVertexArrayOES vao
        return vao


--------------------------------------------------------------------------------
--  delete

delBuf :: GLuint -> IO ()
delBuf buf = do
    alloca $ \ptr -> do
        poke ptr buf
        glDeleteBuffers 1 ptr


delFBO :: GLuint -> IO ()
delFBO fbo =
    alloca $ \ptr -> do
        poke ptr fbo
        glDeleteFramebuffers 1 ptr

delRBO :: GLuint -> IO ()
delRBO rbo =
    alloca $ \ptr -> do
        poke ptr rbo
        glDeleteRenderbuffers 1 ptr

delVAO :: GLuint -> IO ()
delVAO vao =
    alloca $ \ptr -> do
        poke ptr vao
        glDeleteVertexArraysOES 1 ptr

delTex :: GLuint -> IO ()
delTex tex =
    alloca $ \ptr -> do
        poke ptr tex
        glDeleteTextures 1 ptr



--------------------------------------------------------------------------------
--  write buffer

-- | write to buffer. the return value must not rely on the given pointer.
--   fixme: checking errors is probably something that should be done in
--          non-DEBUG programs too...
writeBuf :: GLenum -> (Ptr GLvoid -> IO a) -> IO a
writeBuf target f = do
    glMapBufferOES target gl_WRITE_ONLY_OES >>= \ptr -> do

#ifdef DEBUG
        when (ptr == nullPtr) $ error "writeBuf: nullPtr from glMapBufferOES"
        a <- f ptr
        glUnmapBufferOES target >>= \ok -> case ok of
            0   -> error "writeBuf: false result from glUnmapBufferOES"
            _   -> return a
#else
        a <- f ptr
        glUnmapBufferOES target
        return a
#endif




--------------------------------------------------------------------------------
--  createPrg

createPrg :: FilePath -> FilePath -> [(GLuint, String)] -> [(GLuint, String)] -> 
             IO GLuint
createPrg vshpath fshpath attMap texMap = do
    vsh <- createShader gl_VERTEX_SHADER vshpath
    fsh <- createShader gl_FRAGMENT_SHADER fshpath
    prg <- glCreateProgram
    glAttachShader prg vsh
    glAttachShader prg fsh

    -- bind attributes
    forM_ attMap $ \(att, name) -> do
        withCString name $ \cstr -> 
            glBindAttribLocation prg att cstr
    glLinkProgram prg
   
    -- check status.
    alloca $ \ptrStatus -> do 
        glGetProgramiv prg gl_LINK_STATUS ptrStatus
        peek ptrStatus >>= \status -> case status of
#ifdef DEBUG
            0   -> error $ "error while creating program " ++ prgName ++ 
                           ": could not link program"
#endif
            _   -> return ()

    -- bind textures
    forM_ texMap $ \(tex, name) -> do
        withCString name $ \cstr -> do
            glGetUniformLocation prg cstr >>= \loc -> case loc of
              (-1)    -> 
#ifdef DEBUG
                  putStrLn $ "warning for shader program " ++ prgName ++ 
                             " : no such texture unit: " ++ name
#endif
              loc     ->
                  glProgramUniform1iEXT prg loc $ fI tex


    glDetachShader prg fsh
    glDeleteShader fsh
    glDetachShader prg vsh
    glDeleteShader vsh

    return prg
    
    where
      prgName = 
          "(" ++ takeFileName vshpath ++ ", " ++ takeFileName fshpath ++ ")"


--------------------------------------------------------------------------------
--  programmable pipeline object

createPPO :: Maybe FilePath -> Maybe FilePath -> 
             [ (GLuint, String) ] -> [ (GLuint, String) ] -> 
             IO GLuint
createPPO maybeVshPath maybeFshPath attMap texMap = do
    error "fixme: createPPO"


--------------------------------------------------------------------------------
--  shader


createShader :: GLenum -> FilePath -> IO GLuint
createShader tp path =
    C.catch 
        (BS.readFile path >>= \bs -> BS.useAsCString bs $ \cstr -> do
            shader <- glCreateShader tp
            alloca $ \ptr -> do
                poke ptr cstr
                glShaderSource shader 1 ptr nullPtr
            glCompileShader shader
#ifdef DEBUG
            alloca $ \ptr -> do
                glGetShaderiv shader gl_INFO_LOG_LENGTH ptr
                len <- peek ptr
                when (0 < len) $ do
                    putStrLn $ "problems while compiling shader " ++ shaderName ++ ": \n"
                    allocaArray (fI len) $ \ptr' -> do
                        glGetShaderInfoLog shader len ptr ptr'
                        peekCString ptr' >>= putStrLn
            alloca $ \ptr -> do
                glGetShaderiv shader gl_COMPILE_STATUS ptr
                status <- peek ptr
                when (status == 0) $ do
                    glDeleteShader shader
                    error "(shader did not compile)"
#endif

            return shader)
        (\e -> error ("error while compiling shader " ++ shaderName ++ ": \n" ++ 
               show (e :: IOException)))

    where
      shaderName = 
          takeFileName path

--------------------------------------------------------------------------------
--  load texture


#ifdef GRID_PLATFORM_IOS
foreign import ccall unsafe "ios_loadTexPreMult" c_loadTexPreMult
    :: GLenum -> CString -> Ptr GLuint -> Ptr GLuint -> GLenum -> IO CUInt
#endif
#ifdef GRID_PLATFORM_GLFW
foreign import ccall unsafe "glfw_loadTexPreMult" c_loadTexPreMult
    :: GLenum -> CString -> Ptr GLuint -> Ptr GLuint -> GLenum -> IO CUInt
#endif

-- | load texture of type 'target' into currently bound tex, as 'intfmt'
loadTexPreMult :: GLenum -> GLenum -> FilePath -> IO (UInt, UInt)
loadTexPreMult target intfmt path =
    withCString path $ \cstr -> 
      alloca $ \ptrWth ->
        alloca $ \ptrHth -> 
            c_loadTexPreMult target cstr ptrWth ptrHth intfmt >>= \res -> case res of
                0   -> error $ "loadTexPreMult: could not load " ++ takeFileName path
                _   -> do
#ifdef DEBUG
                    debugGLError debugTag 
#endif
                    wth <- peek ptrWth
                    hth <- peek ptrHth
                    return (fI wth, fI hth)

    where
      debugTag = "loadTexPreMult " ++ debugTarget ++ " " ++ debugIntFmt ++ " " ++ 
                 --takeFileName path ++ ": "
                 path ++ ": "
      debugTarget = case target of
          0x0de1    -> "GL_TEXTURE_2D"
          0x8515    -> "GL_TEXTURE_CUBE_MAP_POSITIVE_X"
          0x8516    -> "GL_TEXTURE_CUBE_MAP_NEGATIVE_X"
          0x8517    -> "GL_TEXTURE_CUBE_MAP_POSITIVE_Y"
          0x8518    -> "GL_TEXTURE_CUBE_MAP_NEGATIVE_Y"
          0x8519    -> "GL_TEXTURE_CUBE_MAP_POSITIVE_Z"
          0x851a    -> "GL_TEXTURE_CUBE_MAP_NEGATIVE_Z"
          _         -> show target
      debugIntFmt = 
          show intfmt


--------------------------------------------------------------------------------
--  

-- glDiscardFramebufferEXT is an Apple thing: 
-- https://www.khronos.org/registry/gles/extensions/EXT/EXT_discard_framebuffer.txt
--
discardFramebuffer :: GLenum -> [GLenum] -> IO ()
discardFramebuffer target attchs = do
#ifdef GRID_PLATFORM_IOS
    let len = length attchs
    allocaArray len $ \ptr -> do
        helper attchs 0 ptr 
        glDiscardFramebufferEXT target (fI len) ptr
    where
      helper (a:as) ix ptr = do
          pokeElemOff ptr ix a
          helper as (ix + 1) ptr
      helper [] ix ptr = 
          return ()
#else
    return ()
#endif
