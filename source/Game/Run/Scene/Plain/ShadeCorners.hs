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
module Game.Run.Scene.Plain.ShadeCorners
  (
    shadeCorners2D,
    shadeCorners3D,
    shadeCornersWrite2D,

  ) where

import MyPrelude
import Game
import Game.Run.RunData

import OpenGL
import OpenGL.Helpers





shadeCorners3D :: ShadeCorners -> Float -> Mat4 -> 
                  Float -> Float -> Float -> 
                  Float -> Float -> Float -> Float -> 
                  Float -> Float -> Float -> Float -> IO ()
shadeCorners3D sh alpha projmodv p0 p1 p2 
                                 sizeX x0 x1 x2 
                                 sizeY y0 y1 y2 =  do

    glUseProgram $ shadeCornersPrg sh

    uniformMat4 (shadeCornersUniProjModvMatrix sh) projmodv
    glUniform1f (shadeCornersUniAlpha sh) $ rTF alpha
   
    -- draw
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D $ shadeCornersTex sh

    glBindVertexArrayOES $ shadeCornersVAO3D sh
    
    -- populate pos
    shadeCornersWrite3D sh p0 p1 p2 sizeX x0 x1 x2 sizeY y0 y1 y2

    glDrawArrays gl_TRIANGLE_STRIP 0 14


    
shadeCorners2D :: ShadeCorners -> Float -> Mat4 -> IO ()
shadeCorners2D sh alpha projmodv = do
    glUseProgram $ shadeCornersPrg sh

    uniformMat4 (shadeCornersUniProjModvMatrix sh) projmodv
    glUniform1f (shadeCornersUniAlpha sh) $ rTF alpha
   
    -- draw
    glDisable gl_CULL_FACE
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D $ shadeCornersTex sh
    glBindVertexArrayOES $ shadeCornersVAO2D sh
    glDrawArrays gl_TRIANGLE_STRIP 0 14
  


--------------------------------------------------------------------------------
--  write

-- | write corners for new size
shadeCornersWrite2D :: ShadeCorners -> Float -> Float -> IO ()
shadeCornersWrite2D sh wth hth = do
    let sizeX = hth * valueSceneCornerSize
        sizeY = wth * valueSceneCornerSize
        
        a1x = rTF $ sizeX * wth
        a2x = rTF $ (1 - sizeX) * wth
        a3x = rTF $ wth

        b1y = rTF $ sizeY * hth
        b2y = rTF $ (1 - sizeY) * hth
        b3y = rTF $ hth

    glBindBuffer gl_ARRAY_BUFFER $ shadeCornersVBO2D sh
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
  
        pokeByteOff ptr (0  +  0) (0.0 :: GLfloat)
        pokeByteOff ptr (0  +  4) (b1y :: GLfloat)

        pokeByteOff ptr (16 +  0) (0.0 :: GLfloat)
        pokeByteOff ptr (16 +  4) (0.0 :: GLfloat)

        pokeByteOff ptr (32 +  0) (a1x :: GLfloat)
        pokeByteOff ptr (32 +  4) (0.0 :: GLfloat)

        pokeByteOff ptr (48 +  0) (a3x :: GLfloat)
        pokeByteOff ptr (48 +  4) (0.0 :: GLfloat)

        pokeByteOff ptr (64 +  0) (a2x :: GLfloat)
        pokeByteOff ptr (64 +  4) (0.0 :: GLfloat)

        pokeByteOff ptr (80 +  0) (a3x :: GLfloat)
        pokeByteOff ptr (80 +  4) (b1y :: GLfloat)

        -- begin
        pokeByteOff ptr (96 +  0) (a3x :: GLfloat)
        pokeByteOff ptr (96 +  4) (b1y :: GLfloat)

        pokeByteOff ptr (112+  0) (a3x :: GLfloat)
        pokeByteOff ptr (112+  4) (b2y :: GLfloat)
        -- end

        pokeByteOff ptr (128+  0) (a3x :: GLfloat)
        pokeByteOff ptr (128+  4) (b2y :: GLfloat)

        pokeByteOff ptr (144+  0) (a3x :: GLfloat)
        pokeByteOff ptr (144+  4) (b3y :: GLfloat)

        pokeByteOff ptr (160+  0) (a2x :: GLfloat)
        pokeByteOff ptr (160+  4) (b3y :: GLfloat)

        pokeByteOff ptr (176+  0) (0.0 :: GLfloat)
        pokeByteOff ptr (176+  4) (b3y :: GLfloat)

        pokeByteOff ptr (192+  0) (a1x :: GLfloat)
        pokeByteOff ptr (192+  4) (b3y :: GLfloat)

        pokeByteOff ptr (208+  0) (0.0 :: GLfloat)
        pokeByteOff ptr (208+  4) (b2y :: GLfloat)
        
  

shadeCornersWrite3D :: ShadeCorners -> Float -> Float -> Float -> 
                       Float -> Float -> Float -> Float -> 
                       Float -> Float -> Float -> Float -> IO ()
shadeCornersWrite3D sh p0 p1 p2 
                       sizeX x0 x1 x2 
                       sizeY y0 y1 y2 = do

    let a1x = sizeX * x0
        a1y = sizeX * x1
        a1z = sizeX * x2
        a2x = (1 - sizeX) * x0
        a2y = (1 - sizeX) * x1
        a2z = (1 - sizeX) * x2
        a3x = x0
        a3y = x1
        a3z = x2
        b1x = sizeY * y0
        b1y = sizeY * y1
        b1z = sizeY * y2
        b2x = (1 - sizeY) * y0
        b2y = (1 - sizeY) * y1
        b2z = (1 - sizeY) * y2
        b3x = y0
        b3y = y1
        b3z = y2
    glBindBuffer gl_ARRAY_BUFFER $ shadeCornersVBOPos3D sh
    glBufferData gl_ARRAY_BUFFER 168 nullPtr gl_STREAM_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        pokeByteOff ptr (0  +  0) (rTF $ p0 + b1x :: GLfloat)
        pokeByteOff ptr (0  +  4) (rTF $ p1 + b1y :: GLfloat)
        pokeByteOff ptr (0  +  8) (rTF $ p2 + b1z :: GLfloat)

        pokeByteOff ptr (12 +  0) (rTF $ p0 + 0 :: GLfloat)
        pokeByteOff ptr (12 +  4) (rTF $ p1 + 0 :: GLfloat)
        pokeByteOff ptr (12 +  8) (rTF $ p2 + 0 :: GLfloat)

        pokeByteOff ptr (24 +  0) (rTF $ p0 + a1x :: GLfloat)
        pokeByteOff ptr (24 +  4) (rTF $ p1 + a1y :: GLfloat)
        pokeByteOff ptr (24 +  8) (rTF $ p2 + a1z :: GLfloat)

        pokeByteOff ptr (36 +  0) (rTF $ p0 + a3x :: GLfloat)
        pokeByteOff ptr (36 +  4) (rTF $ p1 + a3y :: GLfloat)
        pokeByteOff ptr (36 +  8) (rTF $ p2 + a3z :: GLfloat)

        pokeByteOff ptr (48 +  0) (rTF $ p0 + a2x :: GLfloat)
        pokeByteOff ptr (48 +  4) (rTF $ p1 + a2y :: GLfloat)
        pokeByteOff ptr (48 +  8) (rTF $ p2 + a2z :: GLfloat)

        pokeByteOff ptr (60 +  0) (rTF $ p0 + a3x + b1x :: GLfloat)
        pokeByteOff ptr (60 +  4) (rTF $ p1 + a3y + b1y :: GLfloat)
        pokeByteOff ptr (60 +  8) (rTF $ p2 + a3z + b1z :: GLfloat)

        -- begin
        pokeByteOff ptr (72 +  0) (rTF $ p0 + a3x + b1x :: GLfloat)
        pokeByteOff ptr (72 +  4) (rTF $ p1 + a3y + b1y :: GLfloat)
        pokeByteOff ptr (72 +  8) (rTF $ p2 + a3z + b1z :: GLfloat)

        pokeByteOff ptr (84 +  0) (rTF $ p0 + a3x + b2x :: GLfloat)
        pokeByteOff ptr (84 +  4) (rTF $ p1 + a3y + b2y :: GLfloat)
        pokeByteOff ptr (84 +  8) (rTF $ p2 + a3z + b2z :: GLfloat)
        -- end

        pokeByteOff ptr (96 +  0) (rTF $ p0 + a3x + b2x :: GLfloat)
        pokeByteOff ptr (96 +  4) (rTF $ p1 + a3y + b2y :: GLfloat)
        pokeByteOff ptr (96 +  8) (rTF $ p2 + a3z + b2z :: GLfloat)

        pokeByteOff ptr (108+  0) (rTF $ p0 + a3x + b3x :: GLfloat)
        pokeByteOff ptr (108+  4) (rTF $ p1 + a3y + b3y :: GLfloat)
        pokeByteOff ptr (108+  8) (rTF $ p2 + a3z + b3z :: GLfloat)

        pokeByteOff ptr (120+  0) (rTF $ p0 + a2x + b3x :: GLfloat)
        pokeByteOff ptr (120+  4) (rTF $ p1 + a2y + b3y :: GLfloat)
        pokeByteOff ptr (120+  8) (rTF $ p2 + a2z + b3z :: GLfloat)

        pokeByteOff ptr (132+  0) (rTF $ p0 + b3x :: GLfloat)
        pokeByteOff ptr (132+  4) (rTF $ p1 + b3y :: GLfloat)
        pokeByteOff ptr (132+  8) (rTF $ p2 + b3z :: GLfloat)

        pokeByteOff ptr (144+  0) (rTF $ p0 + a1x + b3x :: GLfloat)
        pokeByteOff ptr (144+  4) (rTF $ p1 + a1y + b3y :: GLfloat)
        pokeByteOff ptr (144+  8) (rTF $ p2 + a1z + b3z :: GLfloat)

        pokeByteOff ptr (156+  0) (rTF $ p0 + b2x :: GLfloat)
        pokeByteOff ptr (156+  4) (rTF $ p1 + b2y :: GLfloat)
        pokeByteOff ptr (156+  8) (rTF $ p2 + b2z :: GLfloat)
