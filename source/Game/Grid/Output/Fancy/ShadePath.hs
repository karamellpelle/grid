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
module Game.Grid.Output.Fancy.ShadePath
  (
    shadePathBegin,
    shadePathEnd,

    shadePathAlpha,
    shadePathColor,
    shadePathRadius,

    shadePathDraw,
    shadePathDrawBegin,
    shadePathDrawBeginEnd,

    shadePathDrawPath0,
    shadePathDrawPath0Begin,
    shadePathDrawPath0BeginEnd,

    shadePathDrawPath1,
    shadePathDrawSegment,
    shadePathDrawLine,

  ) where

import MyPrelude
import Game
import Game.Grid
import Game.Data.Color

import OpenGL
import OpenGL.Helpers

-- fixme: begin relative to pathArrayBegin/End!!


--------------------------------------------------------------------------------
--  begin/end shadePath


shadePathBegin :: ShadePath -> Float -> Mat4 -> IO ()
shadePathBegin sh alpha projmodv = do
    glUseProgram $ shadePathPrg sh

    glDepthMask gl_FALSE
    glBlendFuncSeparate gl_ONE gl_ONE
                        gl_ZERO gl_ONE

    -- vao
    glBindVertexArrayOES $ shadePathVAO sh

    -- projmodv matrix
    uniformMat4 (shadePathUniProjModvMatrix sh) projmodv

    -- alpha
    glUniform1f (shadePathUniAlpha sh) $ rTF alpha

    -- bind the canonical texture
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D $ shadePathTex sh


shadePathEnd :: ShadePath -> IO ()
shadePathEnd sh = do
    -- end blending
    glBlendFuncSeparate gl_ONE gl_ONE_MINUS_SRC_ALPHA
                        gl_ONE gl_ONE_MINUS_SRC_ALPHA

    glDepthMask gl_TRUE



--------------------------------------------------------------------------------
--  parameters

shadePathAlpha :: ShadePath -> Float -> IO ()
shadePathAlpha sh alpha = do
    glUniform1f (shadePathUniAlpha sh) $ rTF alpha

shadePathColor :: ShadePath -> Color -> IO ()
shadePathColor sh color = 
    uniformColor (shadePathUniColor sh) color

shadePathRadius :: ShadePath -> Float -> IO ()
shadePathRadius sh r = 
    glUniform1f (shadePathUniRadius sh) $ rTF r


--------------------------------------------------------------------------------
--  draw Path


shadePathDraw :: ShadePath -> Path -> IO ()
shadePathDraw sh path =
    shadePathDrawBegin sh (pathArrayBegin path) path
    

-- | draw Path beginning from array ix 
shadePathDrawBegin :: ShadePath -> UInt -> Path -> IO ()
shadePathDrawBegin sh begin path = do
    --assertIx "shadePathDrawBegin" path begin -- tmp
    shadePathDrawPath0Begin sh begin path
    shadePathDrawPath1 sh path
   



-- | drawing path in range [ix, ix'), alpha interpolates against ix'
shadePathDrawBeginEnd :: ShadePath -> UInt -> UInt -> Float -> Path -> IO ()
shadePathDrawBeginEnd sh begin end alpha path = do
    --assertIx "shadePathDrawBeginEnd begin" path begin
    --assertIx "shadePathDrawBeginEnd end" path end
    -- path0
    shadePathDrawPath0BeginEnd sh begin end path 

    -- path1
    let current = if end == pathArrayEnd path 
                  then pathCurrent path 
                  else segmentarrayRead (pathArray path) end -- fixme: valid ix?
    shadePathDrawSegment sh current alpha 

--------------------------------------------------------------------------------
--  draw Path0


shadePathDrawPath0 :: ShadePath -> Path -> IO ()
shadePathDrawPath0 sh path =
    shadePathDrawPath0Begin sh (pathArrayBegin path) path


shadePathDrawPath0Begin :: ShadePath -> UInt -> Path -> IO ()
shadePathDrawPath0Begin sh begin path = do
    --assertIx "shadePathDrawPath0Begin" path begin
    shadePathDrawPath0BeginEnd sh begin (pathArrayEnd path) path


shadePathDrawPath0BeginEnd :: ShadePath -> UInt -> UInt -> Path -> IO ()
shadePathDrawPath0BeginEnd sh begin end path = do
    --assertIx "shadePathDrawPath0BeginEnd begin" path begin
    --assertIx "shadePathDrawPath0BeginEnd end" path end

    glBindBuffer gl_ARRAY_BUFFER $ pathoutputGLVBO $ pathPathOutput path
    glVertexAttribPointer attPos 3 gl_SHORT gl_FALSE 16 $ mkPtrGLvoid 0
    glVertexAttribPointer attAntiPos 3 gl_SHORT gl_FALSE 16 $ mkPtrGLvoid 8


    -- 
{-
    let size = pathArraySize path
        abegin = pathArrayBegin path

        begin' = (abegin + begin) `mod` size
        end' = (abegin + end) `mod` size
        end'' = begin' + ((end' + (size - begin')) `mod` size)

        a0 = begin'
        a1 = min end'' size
        b0 = 0
        b1 = (max end'' size) `mod` size
-}
    let size = pathArraySize path

        out = begin + (end + (size - begin)) `mod` size

        a0 = begin
        a1 = min size out
        b0 = 0
        b1 = (max size out) `mod` size
    
    -- tmp
    assertBE "a0 a1" begin end path a0 a1
    assertBE "b0 b1" begin end path b0 b1

    
    -- [a0, a1)
    glDrawElements gl_TRIANGLE_STRIP (fI $ (8 + 2) * (a1 - a0)) gl_UNSIGNED_SHORT 
                   (mkPtrGLvoid (fI $ 2 * (8 + 2) * a0))

    -- [b0, b1)
    glDrawElements gl_TRIANGLE_STRIP (fI $ (8 + 2) * (b1 - b0)) gl_UNSIGNED_SHORT 
                   (mkPtrGLvoid (fI $ 2 * (8 + 2) * b0))

    --debugGLError "shadePath"

    where 
      assertBE tag begin end path b e = do
          let size = pathArraySize path 
          when (size <= b) $ assertErr (tag ++ " (size <= b)")
          when (size < e) $ assertErr (tag ++ " (size < e)")
          where
            assertErr tag = do
                let size = pathArraySize path
                    abegin = pathArrayBegin path

                    begin' = (abegin + begin) `mod` size
                    end' = (abegin + end) `mod` size
                    end'' = begin' + ((end' + (size - begin')) `mod` size)

                    a0 = begin'
                    a1 = min end'' size
                    b0 = 0
                    b1 = (max end'' size) `mod` size
                    pathBegin = pathArrayBegin path
                    pathEnd = pathArrayEnd path
                bufVBO <- getBufferSize gl_ARRAY_BUFFER $ pathoutputGLVBO 
                                                        $ pathPathOutput path
                putStrLn $ "VBO size:       " ++ show bufVBO ++ " (/ 128 == " 
                                              ++ show (div bufVBO 128) ++ ")" 
                putStrLn $ "pathArraySize:  " ++ show size
                putStrLn $ "pathArrayBegin: " ++ show pathBegin
                putStrLn $ "pathArrayEnd:   " ++ show pathEnd
                putStrLn $ "begin:          " ++ show begin
                putStrLn $ "end:            " ++ show end
                putStrLn $ "end':           " ++ show end'
                putStrLn $ "a0:             " ++ show a0
                putStrLn $ "a1:             " ++ show a1
                putStrLn $ "b0:             " ++ show b0
                putStrLn $ "b1:             " ++ show b1
                putStrLn ""
      getBufferSize tgt buf =
          alloca $ \ptr -> do
              glGetBufferParameteriv tgt gl_BUFFER_SIZE ptr
              peek ptr

--------------------------------------------------------------------------------
--  draw Path1

shadePathDrawPath1 :: ShadePath -> Path -> IO ()
shadePathDrawPath1 sh path = do
    shadePathDrawSegment sh (pathCurrent path) (pathAlpha path)


shadePathDrawSegment :: ShadePath -> Segment -> Float -> IO ()
shadePathDrawSegment sh segment alpha = do
    case segment of
        Segment (Node x y z) (Turn a0 a1 a2
                                   _  _  _
                                   _  _  _) ->
            let xf = fI x
                yf = fI y
                zf = fI z
                xf' = smooth xf (xf + fI a0) alpha
                yf' = smooth yf (yf + fI a1) alpha
                zf' = smooth zf (zf + fI a2) alpha
            in  shadePathDrawLine sh xf yf zf xf' yf' zf'


-- | draw a line
shadePathDrawLine :: ShadePath -> Float -> Float -> Float -> 
                                  Float -> Float -> Float -> IO ()
shadePathDrawLine sh x0 y0 z0 x1 y1 z1 = do
    glBindBuffer gl_ARRAY_BUFFER $ shadePathPath1VBO sh
    glBufferData gl_ARRAY_BUFFER (1 * 8 * 24) nullPtr gl_STREAM_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        -- pos
        pokeByteOff ptr 0  (rTF x0 :: GLfloat)
        pokeByteOff ptr 4  (rTF y0 :: GLfloat)
        pokeByteOff ptr 8  (rTF z0 :: GLfloat)
        pokeByteOff ptr 12 (rTF x1  :: GLfloat)
        pokeByteOff ptr 16 (rTF y1  :: GLfloat)
        pokeByteOff ptr 20 (rTF z1  :: GLfloat)

        pokeByteOff ptr 24 (rTF x0 :: GLfloat)
        pokeByteOff ptr 28 (rTF y0 :: GLfloat)
        pokeByteOff ptr 32 (rTF z0 :: GLfloat)
        pokeByteOff ptr 36 (rTF x1 :: GLfloat)
        pokeByteOff ptr 40 (rTF y1 :: GLfloat)
        pokeByteOff ptr 44 (rTF z1 :: GLfloat)

        pokeByteOff ptr 48 (rTF x0 :: GLfloat)
        pokeByteOff ptr 52 (rTF y0 :: GLfloat)
        pokeByteOff ptr 56 (rTF z0 :: GLfloat)
        pokeByteOff ptr 60 (rTF x1 :: GLfloat)
        pokeByteOff ptr 64 (rTF y1 :: GLfloat)
        pokeByteOff ptr 68 (rTF z1 :: GLfloat)

        pokeByteOff ptr 72 (rTF x0 :: GLfloat)
        pokeByteOff ptr 76 (rTF y0 :: GLfloat)
        pokeByteOff ptr 80 (rTF z0 :: GLfloat)
        pokeByteOff ptr 84 (rTF x1 :: GLfloat)
        pokeByteOff ptr 88 (rTF y1 :: GLfloat)
        pokeByteOff ptr 92 (rTF z1 :: GLfloat)

        -- pos'
        pokeByteOff ptr 96 (rTF x1 :: GLfloat)
        pokeByteOff ptr 100 (rTF y1 :: GLfloat)
        pokeByteOff ptr 104 (rTF z1 :: GLfloat)
        pokeByteOff ptr 108 (rTF x0 :: GLfloat)
        pokeByteOff ptr 112 (rTF y0 :: GLfloat)
        pokeByteOff ptr 116 (rTF z0 :: GLfloat)

        pokeByteOff ptr 120 (rTF x1 :: GLfloat)
        pokeByteOff ptr 124 (rTF y1 :: GLfloat)
        pokeByteOff ptr 128 (rTF z1 :: GLfloat)
        pokeByteOff ptr 132 (rTF x0 :: GLfloat)
        pokeByteOff ptr 136 (rTF y0 :: GLfloat)
        pokeByteOff ptr 140 (rTF z0 :: GLfloat)

        pokeByteOff ptr 144 (rTF x1 :: GLfloat)
        pokeByteOff ptr 148 (rTF y1 :: GLfloat)
        pokeByteOff ptr 152 (rTF z1 :: GLfloat)
        pokeByteOff ptr 156 (rTF x0 :: GLfloat)
        pokeByteOff ptr 160 (rTF y0 :: GLfloat)
        pokeByteOff ptr 164 (rTF z0 :: GLfloat)

        pokeByteOff ptr 168 (rTF x1 :: GLfloat)
        pokeByteOff ptr 172 (rTF y1 :: GLfloat)
        pokeByteOff ptr 176 (rTF z1 :: GLfloat)
        pokeByteOff ptr 180 (rTF x0 :: GLfloat)
        pokeByteOff ptr 184 (rTF y0 :: GLfloat)
        pokeByteOff ptr 188 (rTF z0 :: GLfloat)


    glVertexAttribPointer attPos 3 gl_FLOAT gl_FALSE 24 $ mkPtrGLvoid 0
    glVertexAttribPointer attAntiPos 3 gl_FLOAT gl_FALSE 24 $ mkPtrGLvoid 12

    -- draw!
    glDrawElements gl_TRIANGLE_STRIP 8 gl_UNSIGNED_SHORT nullPtr



--------------------------------------------------------------------------------
--  

{-
-- tmp
assertIx :: String -> Path -> UInt -> IO ()
assertIx tag path ix = 
    when (pathArraySize path <= ix) $ do
       putStrLn $ tag ++ "assertIx with pathArraySize: " ++ show (pathArraySize path) ++
                  ", ix: " ++ show ix

-}
