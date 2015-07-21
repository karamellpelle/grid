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
module OpenAL.Helpers
  (
    genBuf,
    delBuf,
    loadBuf,

    genSrc,
    delSrc,

    getSrcState,

    listenerMat4,

    module Linear,
  ) where


import MyPrelude
import System.FilePath

import OpenAL
import Linear

--------------------------------------------------------------------------------
--  buffer

genBuf :: IO ALuint
genBuf = 
    alloca $ \ptr -> do
        alGenBuffers 1 ptr
        peek ptr

delBuf :: ALuint -> IO ()
delBuf src = 
    alloca $ \ptr -> do
        poke ptr src
        alDeleteBuffers 1 ptr

loadBuf :: ALuint -> FilePath -> IO ()
loadBuf buf path =
    withCString path $ \cstr -> 
        c_loadBuf buf cstr >>= \res -> case res of
            0   -> error $ "loadBuf: could not load " ++ takeFileName path
            _   -> return ()


#ifdef GRID_PLATFORM_IOS
foreign import ccall unsafe "ios_loadBuf" c_loadBuf 
    :: ALuint -> CString -> IO CUInt
#endif
#ifdef GRID_PLATFORM_GLFW
foreign import ccall unsafe "glfw_loadBuf" c_loadBuf 
    :: ALuint -> CString -> IO CUInt
#endif


--------------------------------------------------------------------------------
--  source

genSrc :: IO ALuint
genSrc = 
    alloca $ \ptr -> do
        alGenSources 1 ptr
        peek ptr

delSrc :: ALuint -> IO ()
delSrc src = 
    alloca $ \ptr -> do
        poke ptr src
        alDeleteSources 1 ptr


getSrcState :: ALuint -> IO ALenum
getSrcState src = 
    alloca $ \ptr -> do
        alGetSourcei src al_SOURCE_STATE ptr
        fmap fI $ peek ptr


--------------------------------------------------------------------------------
--  

-- | set listener position and orientation from modelview matrix.
--   note: * velocity of listener is not set here
--         * not sure if this function is working properly
--   fixme: use projmodv instead?
listenerMat4 :: Mat4 -> IO ()
listenerMat4 (Mat4 a0 a1 a2 a3
                   b0 b1 b2 b3
                   c0 c1 c2 c3
                   d0 d1 d2 d3) = do
    -- pos
    alListener3f al_POSITION (-(rTF d0)) (-(rTF d1)) (-(rTF d2))
    
    -- dir, up
    allocaBytes 36 $ \ptr -> do
        pokeByteOff ptr 0  (-rTF a2 :: ALfloat) 
        pokeByteOff ptr 4  (-rTF b2 :: ALfloat) 
        pokeByteOff ptr 8  (-rTF c2 :: ALfloat) 
        pokeByteOff ptr 12 (rTF a1 :: ALfloat) 
        pokeByteOff ptr 16 (rTF b1 :: ALfloat) 
        pokeByteOff ptr 20 (rTF c1 :: ALfloat) 
        alListenerfv al_ORIENTATION ptr


