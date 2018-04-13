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
module Game.Grid.GridWorld.Path.Plain
  (
    PathOutput (..),

    makePathOutput,
    clearPathOutput,
    destroyPathOutput,
    pushPathOutput,

  ) where

import MyPrelude
import Game

import Game.Grid.GridWorld.Segment
import Game.Grid.GridWorld.Node
import Game.Grid.GridWorld.Turn

import OpenGL
import OpenGL.Helpers


data PathOutput =
    PathOutput
    {
        pathoutputGLVBO :: !GLuint
    }



makePathOutput :: UInt -> MEnv' PathOutput
makePathOutput pathsize = io $ do
    vbo <- bindNewBuf gl_ARRAY_BUFFER
    let bytesize = pathsize * 128
    glBufferData gl_ARRAY_BUFFER (fI bytesize) nullPtr gl_DYNAMIC_DRAW
    return  PathOutput
            {
                pathoutputGLVBO = vbo
            }

clearPathOutput :: PathOutput -> MEnv' PathOutput
clearPathOutput po = 
    return  po


destroyPathOutput :: PathOutput -> MEnv' ()
destroyPathOutput path = io $ do
    delBuf $ pathoutputGLVBO path


-- fixme: this does not work if (Size <= End - GLEnd) 
--        (i.e. stepDT increase segments of path too much before output)
pushPathOutput :: PathOutput -> UInt -> UInt -> Segment -> IO PathOutput
pushPathOutput po size end (Segment (Node x y z) (Turn a0 a1 a2 _ _ _ _ _ _)) = do
    let off = fI $ end * 128
        x' = fI x + fI a0 :: GLshort
        y' = fI y + fI a1 :: GLshort
        z' = fI z + fI a2 :: GLshort

    glBindBuffer gl_ARRAY_BUFFER $ pathoutputGLVBO po
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        -- pos
        pokeByteOff ptr (off +  0) x
        pokeByteOff ptr (off +  2) y
        pokeByteOff ptr (off +  4) z
        pokeByteOff ptr (off +  8) x'
        pokeByteOff ptr (off + 10) y'
        pokeByteOff ptr (off + 12) z'
        
        pokeByteOff ptr (off + 16) x
        pokeByteOff ptr (off + 18) y
        pokeByteOff ptr (off + 20) z
        pokeByteOff ptr (off + 24) x'
        pokeByteOff ptr (off + 26) y'
        pokeByteOff ptr (off + 28) z'
        
        pokeByteOff ptr (off + 32) x
        pokeByteOff ptr (off + 34) y
        pokeByteOff ptr (off + 36) z
        pokeByteOff ptr (off + 40) x'
        pokeByteOff ptr (off + 42) y'
        pokeByteOff ptr (off + 44) z'
        
        pokeByteOff ptr (off + 48) x
        pokeByteOff ptr (off + 50) y
        pokeByteOff ptr (off + 52) z
        pokeByteOff ptr (off + 56) x'
        pokeByteOff ptr (off + 58) y'
        pokeByteOff ptr (off + 60) z'
        
        -- pos'
        pokeByteOff ptr (off + 64) x'
        pokeByteOff ptr (off + 66) y'
        pokeByteOff ptr (off + 68) z'
        pokeByteOff ptr (off + 72) x
        pokeByteOff ptr (off + 74) y
        pokeByteOff ptr (off + 76) z

        pokeByteOff ptr (off + 80) x'
        pokeByteOff ptr (off + 82) y'
        pokeByteOff ptr (off + 84) z'
        pokeByteOff ptr (off + 88) x
        pokeByteOff ptr (off + 90) y
        pokeByteOff ptr (off + 92) z

        pokeByteOff ptr (off + 96) x'
        pokeByteOff ptr (off + 98) y'
        pokeByteOff ptr (off +100) z'
        pokeByteOff ptr (off +104) x
        pokeByteOff ptr (off +106) y
        pokeByteOff ptr (off +108) z

        pokeByteOff ptr (off +112) x'
        pokeByteOff ptr (off +114) y'
        pokeByteOff ptr (off +116) z'
        pokeByteOff ptr (off +120) x
        pokeByteOff ptr (off +122) y
        pokeByteOff ptr (off +124) z

    return po 
