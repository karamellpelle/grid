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
module Game.Run.Output.Fancy.CornerData
  (
    cornerdataWrite3D,
    cornerdataWrite2D,

  ) where

import MyPrelude
import File

import Game
import Game.Data.Shape
import Game.Run.RunData.Fancy.CornerData

import OpenGL
import OpenGL.Helpers



--------------------------------------------------------------------------------
--  write data



cornerdataWrite3D :: CornerData -> 
                     Float -> Float -> Float -> 
                     Float -> Float -> Float -> Float -> 
                     Float -> Float -> Float -> Float -> IO ()
cornerdataWrite3D cornerdata p0 p1 p2 
                             sizeX x0 x1 x2 
                             sizeY y0 y1 y2 =  do

    let px = rTF p0 :: GLfloat
        py = rTF p1 :: GLfloat 
        pz = rTF p2 :: GLfloat
        a1x = rTF $ sizeX * x0 :: GLfloat
        a1y = rTF $ sizeX * x1 :: GLfloat
        a1z = rTF $ sizeX * x2 :: GLfloat
        a2x = rTF $ (1 - sizeX) * x0 :: GLfloat
        a2y = rTF $ (1 - sizeX) * x1 :: GLfloat
        a2z = rTF $ (1 - sizeX) * x2 :: GLfloat
        a3x = rTF x0 :: GLfloat
        a3y = rTF x1 :: GLfloat 
        a3z = rTF x2 :: GLfloat 
        b1x = rTF $ sizeY * y0 :: GLfloat 
        b1y = rTF $ sizeY * y1 :: GLfloat 
        b1z = rTF $ sizeY * y2 :: GLfloat 
        b2x = rTF $ (1 - sizeY) * y0 :: GLfloat
        b2y = rTF $ (1 - sizeY) * y1 :: GLfloat
        b2z = rTF $ (1 - sizeY) * y2 :: GLfloat
        b3x = rTF y0 :: GLfloat
        b3y = rTF y1 :: GLfloat
        b3z = rTF y2 :: GLfloat

    glBindBuffer gl_ARRAY_BUFFER $ cornerdataVBO3D cornerdata
    glBufferData gl_ARRAY_BUFFER 288 nullPtr gl_STREAM_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        -- 0 
        pokeByteOff ptr 0   (px + b3x) 
        pokeByteOff ptr 4   (py + b3y) 
        pokeByteOff ptr 8   (pz + b3z) 
        
        pokeByteOff ptr 12  (px + b2x) 
        pokeByteOff ptr 16  (py + b2y) 
        pokeByteOff ptr 20  (pz + b2z) 
        
        pokeByteOff ptr 24  (px + a1x + b3x) 
        pokeByteOff ptr 28  (py + a1y + b3y) 
        pokeByteOff ptr 32  (pz + a1z + b3z) 
        
        pokeByteOff ptr 36  (px + a1x + b2x) 
        pokeByteOff ptr 40  (py + a1y + b2y) 
        pokeByteOff ptr 44  (pz + a1z + b2z) 

        pokeByteOff ptr 48  (px + a1x + b2x) 
        pokeByteOff ptr 52  (py + a1y + b2y) 
        pokeByteOff ptr 56  (pz + a1z + b2z) 
        pokeByteOff ptr 60  (px + a2x + b3x) 
        pokeByteOff ptr 64  (py + a2y + b3y) 
        pokeByteOff ptr 68  (pz + a2z + b3z) 
        
        
        -- 1
        pokeByteOff ptr 72  (px + a2x + b3x) 
        pokeByteOff ptr 76  (py + a2y + b3y) 
        pokeByteOff ptr 80  (pz + a2z + b3z) 
        
        pokeByteOff ptr 84  (px + a2x + b2x) 
        pokeByteOff ptr 88  (py + a2y + b2y) 
        pokeByteOff ptr 92  (pz + a2z + b2z) 
        
        pokeByteOff ptr 96  (px + a3x + b3x) 
        pokeByteOff ptr 100 (py + a3y + b3y) 
        pokeByteOff ptr 104 (pz + a3z + b3z) 
        
        pokeByteOff ptr 108 (px + a3x + b2x) 
        pokeByteOff ptr 112 (py + a3y + b2y) 
        pokeByteOff ptr 116 (pz + a3z + b2z) 

        pokeByteOff ptr 120 (px + a3x + b2x) 
        pokeByteOff ptr 124 (py + a3y + b2y) 
        pokeByteOff ptr 128 (pz + a3z + b2z) 
        pokeByteOff ptr 132 (px + b1x) 
        pokeByteOff ptr 136 (py + b1y) 
        pokeByteOff ptr 140 (pz + b1z) 
        
        
        -- 2
        pokeByteOff ptr 144 (px + b1x) 
        pokeByteOff ptr 148 (py + b1y) 
        pokeByteOff ptr 152 (pz + b1z) 
        
        pokeByteOff ptr 156 (px) 
        pokeByteOff ptr 160 (py) 
        pokeByteOff ptr 164 (pz) 
        
        pokeByteOff ptr 168 (px + a1x + b1x) 
        pokeByteOff ptr 172 (py + a1y + b1y) 
        pokeByteOff ptr 176 (pz + a1z + b1z) 
        
        pokeByteOff ptr 180 (px + a1x) 
        pokeByteOff ptr 184 (py + a1y) 
        pokeByteOff ptr 188 (pz + a1z) 

        pokeByteOff ptr 192 (px + a1x) 
        pokeByteOff ptr 196 (py + a1y) 
        pokeByteOff ptr 200 (pz + a1z) 
        pokeByteOff ptr 204 (px + a2x + b1x) 
        pokeByteOff ptr 208 (py + a2y + b1y) 
        pokeByteOff ptr 212 (pz + a2z + b1z) 
        
        
        -- 3
        pokeByteOff ptr 216 (px + a2x + b1x) 
        pokeByteOff ptr 220 (py + a2y + b1y) 
        pokeByteOff ptr 224 (pz + a2z + b1z) 
        
        pokeByteOff ptr 228 (px + a2x) 
        pokeByteOff ptr 232 (py + a2y) 
        pokeByteOff ptr 236 (pz + a2z) 
        
        pokeByteOff ptr 240 (px + a3x + b1x) 
        pokeByteOff ptr 244 (py + a3y + b1y) 
        pokeByteOff ptr 248 (pz + a3z + b1z) 
        
        pokeByteOff ptr 252 (px + a3x) 
        pokeByteOff ptr 256 (py + a3y) 
        pokeByteOff ptr 260 (pz + a3z) 

        pokeByteOff ptr 264 (px + a3x) 
        pokeByteOff ptr 268 (py + a3y) 
        pokeByteOff ptr 272 (pz + a3z) 
        pokeByteOff ptr 276 (px + a3x) 
        pokeByteOff ptr 280 (py + a3y) 
        pokeByteOff ptr 284 (pz + a3z) 
       
    

    
-- | write corners for new size
cornerdataWrite2D :: CornerData -> Shape -> IO ()
cornerdataWrite2D cornerdata (Shape wth hth) = do
    let sizeX = hth * valueSceneCornerSize
        sizeY = wth * valueSceneCornerSize
        
        x0 = 0.0 :: GLfloat
        x1 = rTF $ sizeX * wth :: GLfloat
        x2 = rTF $ (1 - sizeX) * wth :: GLfloat
        x3 = rTF $ wth :: GLfloat
        y0 = 0.0 :: GLfloat
        y1 = rTF $ sizeY * hth :: GLfloat
        y2 = rTF $ (1 - sizeY) * hth :: GLfloat
        y3 = rTF $ hth :: GLfloat

    glBindBuffer gl_ARRAY_BUFFER $ cornerdataVBO2D cornerdata
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        -- 0 
        pokeByteOff ptr 0   x0
        pokeByteOff ptr 4   y0
        
        pokeByteOff ptr 12  x0
        pokeByteOff ptr 16  y1
        
        pokeByteOff ptr 24  x1
        pokeByteOff ptr 28  y0
        
        pokeByteOff ptr 36  x1
        pokeByteOff ptr 40  y1
  
        pokeByteOff ptr 48  x1
        pokeByteOff ptr 52  y1
        pokeByteOff ptr 60  x2
        pokeByteOff ptr 64  y0
  

        -- 1
        pokeByteOff ptr 72  x2
        pokeByteOff ptr 76  y0
        
        pokeByteOff ptr 84  x2
        pokeByteOff ptr 88  y1
        
        pokeByteOff ptr 96  x3
        pokeByteOff ptr 100 y0
        
        pokeByteOff ptr 108 x3
        pokeByteOff ptr 112 y1
        
        pokeByteOff ptr 120 x3
        pokeByteOff ptr 124 y1
        pokeByteOff ptr 132 x0
        pokeByteOff ptr 136 y2
          

        -- 2
        pokeByteOff ptr 144 x0
        pokeByteOff ptr 148 y2
        
        pokeByteOff ptr 156 x0
        pokeByteOff ptr 160 y3
        
        pokeByteOff ptr 168 x1
        pokeByteOff ptr 172 y2
        
        pokeByteOff ptr 180 x1
        pokeByteOff ptr 184 y3

        pokeByteOff ptr 192 x1
        pokeByteOff ptr 196 y3
        pokeByteOff ptr 204 x2
        pokeByteOff ptr 208 y2
  

        -- 3
        pokeByteOff ptr 216 x2
        pokeByteOff ptr 220 y2
        
        pokeByteOff ptr 228 x2
        pokeByteOff ptr 232 y3
        
        pokeByteOff ptr 240 x3
        pokeByteOff ptr 244 y2
        
        pokeByteOff ptr 252 x3
        pokeByteOff ptr 256 y3
       
        pokeByteOff ptr 264 x3
        pokeByteOff ptr 268 y3
        pokeByteOff ptr 276 x3
        pokeByteOff ptr 280 y3



