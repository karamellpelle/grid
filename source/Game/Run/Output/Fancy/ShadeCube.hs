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
module Game.Run.Output.Fancy.ShadeCube
  (
    shadeCube,
    shadeCubeWrite,

  ) where

import MyPrelude
import Game

import Game.Run.RunWorld
import Game.Run.RunData.Fancy.ShadeCube

import OpenGL
import OpenGL.Helpers



shadeCube :: ShadeCube -> Float -> Mat4 -> Mat4 -> RunWorld -> IO ()
shadeCube sh alpha projmodv normal run =  do

    -- begin --
    glUseProgram $ shadeCubePrg sh
    glDisable gl_CULL_FACE

    uniformMat4 (shadeCubeUniProjModvMatrix sh) projmodv
    uniformMat4AsMat3 (shadeCubeUniNormalMatrix sh) normal

    -- alpha 
    glUniform1f (shadeCubeUniAlpha sh) $ rTF alpha


    -- draw
    glBindVertexArrayOES $ shadeCubeVAO sh
    glActiveTexture gl_TEXTURE0

    -- LevelMode + PuzzleMode
    glBindTexture gl_TEXTURE_2D $ screenshotTex $ runLevelPuzzleScreenshot run
    glDrawArrays gl_TRIANGLE_STRIP 0  4
    glDrawArrays gl_TRIANGLE_STRIP 8  4

    -- Foreign
    glBindTexture gl_TEXTURE_2D $ screenshotTex $ runForeignScreenshot run
    glDrawArrays gl_TRIANGLE_STRIP 4  4

    -- MemoryMode
    glBindTexture gl_TEXTURE_2D $ screenshotTex $ runMemoryScreenshot run
    glDrawArrays gl_TRIANGLE_STRIP 12 4

    -- end --
    glEnable gl_CULL_FACE





-- | write cube after shape
shadeCubeWrite :: ShadeCube -> Shape -> IO ()
shadeCubeWrite sh (Shape wth hth) = do
    glBindBuffer gl_ARRAY_BUFFER $ shadeCubeVBODynamic sh
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        let x = rTF $ valueRunCubeRadius * wth :: GLfloat
            y = x * rTF hth :: GLfloat
            z = x :: GLfloat
            s0 = normGLushort $ 0.5 * (1.0 - wth)
            t0 = normGLushort $ 0.5 * (1.0 - hth)
            s1 = 0xffff - s0
            t1 = 0xffff - t0
        --
        pokeByteOff ptr (0   +  0) (-x)
        pokeByteOff ptr (0   +  4) (y)
        pokeByteOff ptr (0   +  8) (-z)
        pokeByteOff ptr (0   +  16) s0
        pokeByteOff ptr (0   +  18) t1

        pokeByteOff ptr (20  +  0) (-x)
        pokeByteOff ptr (20  +  4) (-y)
        pokeByteOff ptr (20  +  8) (-z)
        pokeByteOff ptr (20  +  16) s0
        pokeByteOff ptr (20  +  18) t0
        
        pokeByteOff ptr (40  +  0) (-x)
        pokeByteOff ptr (40  +  4) (y)
        pokeByteOff ptr (40  +  8) (z)
        pokeByteOff ptr (40  +  16) s1
        pokeByteOff ptr (40  +  18) t1
        
        pokeByteOff ptr (60  +  0) (-x)
        pokeByteOff ptr (60  +  4) (-y)
        pokeByteOff ptr (60  +  8) (z)
        pokeByteOff ptr (60  +  16) s1
        pokeByteOff ptr (60  +  18) t0
       
        --
        pokeByteOff ptr (80  +  0) (-x)
        pokeByteOff ptr (80  +  4) (y)
        pokeByteOff ptr (80  +  8) (z)
        pokeByteOff ptr (80  +  16) s0
        pokeByteOff ptr (80  +  18) t1
        
        pokeByteOff ptr (100 +  0) (-x)
        pokeByteOff ptr (100 +  4) (-y)
        pokeByteOff ptr (100 +  8) (z)
        pokeByteOff ptr (100 +  16) s0
        pokeByteOff ptr (100 +  18) t0
        
        pokeByteOff ptr (120 +  0) (x)
        pokeByteOff ptr (120 +  4) (y)
        pokeByteOff ptr (120 +  8) (z)
        pokeByteOff ptr (120 +  16) s1
        pokeByteOff ptr (120 +  18) t1
        
        pokeByteOff ptr (140 +  0) (x)
        pokeByteOff ptr (140 +  4) (-y)
        pokeByteOff ptr (140 +  8) (z)
        pokeByteOff ptr (140 +  16) s1
        pokeByteOff ptr (140 +  18) t0
       
        --
        pokeByteOff ptr (160 +  0) (x)
        pokeByteOff ptr (160 +  4) (y)
        pokeByteOff ptr (160 +  8) (z)
        pokeByteOff ptr (160 +  16) s0
        pokeByteOff ptr (160 +  18) t1
        
        pokeByteOff ptr (180 +  0) (x)
        pokeByteOff ptr (180 +  4) (-y)
        pokeByteOff ptr (180 +  8) (z)
        pokeByteOff ptr (180 +  16) s0
        pokeByteOff ptr (180 +  18) t0
        
        pokeByteOff ptr (200 +  0) (x)
        pokeByteOff ptr (200 +  4) (y)
        pokeByteOff ptr (200 +  8) (-z)
        pokeByteOff ptr (200 +  16) s1
        pokeByteOff ptr (200 +  18) t1
        
        pokeByteOff ptr (220 +  0) (x)
        pokeByteOff ptr (220 +  4) (-y)
        pokeByteOff ptr (220 +  8) (-z)
        pokeByteOff ptr (220 +  16) s1
        pokeByteOff ptr (220 +  18) t0
      
        --
        pokeByteOff ptr (240 +  0) (x)
        pokeByteOff ptr (240 +  4) (y)
        pokeByteOff ptr (240 +  8) (-z)
        pokeByteOff ptr (240 +  16) s0
        pokeByteOff ptr (240 +  18) t1
        
        pokeByteOff ptr (260 +  0) (x)
        pokeByteOff ptr (260 +  4) (-y)
        pokeByteOff ptr (260 +  8) (-z)
        pokeByteOff ptr (260 +  16) s0
        pokeByteOff ptr (260 +  18) t0
        
        pokeByteOff ptr (280 +  0) (-x)
        pokeByteOff ptr (280 +  4) (y)
        pokeByteOff ptr (280 +  8) (-z)
        pokeByteOff ptr (280 +  16) s1
        pokeByteOff ptr (280 +  18) t1
        
        pokeByteOff ptr (300 +  0) (-x)
        pokeByteOff ptr (300 +  4) (-y)
        pokeByteOff ptr (300 +  8) (-z)
        pokeByteOff ptr (300 +  16) s1
        pokeByteOff ptr (300 +  18) t0

    return () 



