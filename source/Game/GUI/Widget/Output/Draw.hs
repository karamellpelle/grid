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
module Game.GUI.Widget.Output.Draw
  (
    draw24,
    draw24ShapeSubSize,
    draw24ShapeAddSize,

    draw8,
    draw8ShapeSubSize,
    draw8ShapeAddSize,
    draw8ShapeSub,
    draw8ShapeAdd,

    draw4,
    draw4Shape,
    draw4ShapeTex,

    drawTexts,
    drawCenteredTexts,
    
    module Game.Font,
  ) where


import MyPrelude

import Game.Font
import Game.GUI.GUIShade
import Game.GUI.Widget
import Game.GUI.Widget.Helpers

import OpenGL
import OpenGL.Helpers



-- writeVBOXXX:     write VBO
-- drawVBOXXX:      draw VBO
-- drawXXX:         draw default VBO (assuming prewritten shape)
-- drawXXXShape:    write default VBO, draw default VBO

--------------------------------------------------------------------------------
--  VBO24


-- | assuming VBO of 24 * (2 * float).
writeVBO24ShapeSubSize :: GUIData -> GUIState -> GLuint -> GUIShape -> Float -> IO () 
writeVBO24ShapeSubSize gd gs vbo (GUIShape wth hth) size = do
    let x0 = 0.0 :: GLfloat
        x1 = rTF size :: GLfloat 
        x2 = rTF (wth - size) :: GLfloat
        x3 = rTF wth :: GLfloat 
        y0 = 0.0 :: GLfloat
        y1 = rTF size :: GLfloat 
        y2 = rTF (hth - size) :: GLfloat
        y3 = rTF hth :: GLfloat
    glBindBuffer gl_ARRAY_BUFFER vbo
    glBufferData gl_ARRAY_BUFFER 192 nullPtr gl_STREAM_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> 
        writeVBO24 x0 x1 x2 x3 y0 y1 y2 y3 ptr



-- | assuming vbo of 24 * (2 * float).
writeVBO24ShapeAddSize :: GUIData -> GUIState -> GLuint -> GUIShape -> Float -> IO () 
writeVBO24ShapeAddSize gd gs vbo (GUIShape wth hth) size = do
    let x0 = rTF (-size) :: GLfloat
        x1 = 0.0 :: GLfloat 
        x2 = rTF wth :: GLfloat
        x3 = rTF (wth + size) :: GLfloat 
        y0 = rTF (-size) :: GLfloat
        y1 = 0.0 :: GLfloat 
        y2 = rTF hth :: GLfloat
        y3 = rTF (hth + size) :: GLfloat
    glBindBuffer gl_ARRAY_BUFFER vbo
    glBufferData gl_ARRAY_BUFFER 192 nullPtr gl_STREAM_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr ->
        writeVBO24 x0 x1 x2 x3 y0 y1 y2 y3 ptr



writeVBO24 :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> 
              GLfloat -> GLfloat -> GLfloat -> GLfloat -> Ptr a -> IO ()
writeVBO24 x0 x1 x2 x3 y0 y1 y2 y3 ptr = do
    pokeByteOff ptr (0  +  0) x0
    pokeByteOff ptr (0  +  4) y0
    pokeByteOff ptr (8  +  0) x0
    pokeByteOff ptr (8  +  4) y1
    pokeByteOff ptr (16 +  0) x1
    pokeByteOff ptr (16 +  4) y0
    pokeByteOff ptr (24 +  0) x1
    pokeByteOff ptr (24 +  4) y1
    pokeByteOff ptr (32 +  0) x2
    pokeByteOff ptr (32 +  4) y0
    pokeByteOff ptr (40 +  0) x2
    pokeByteOff ptr (40 +  4) y1
    pokeByteOff ptr (48 +  0) x3
    pokeByteOff ptr (48 +  4) y0
    pokeByteOff ptr (56 +  0) x3
    pokeByteOff ptr (56 +  4) y1

    pokeByteOff ptr (64 +  0) x3
    pokeByteOff ptr (64 +  4) y2
    pokeByteOff ptr (72 +  0) x3
    pokeByteOff ptr (72 +  4) y1
    pokeByteOff ptr (80 +  0) x2
    pokeByteOff ptr (80 +  4) y2
    pokeByteOff ptr (88 +  0) x2
    pokeByteOff ptr (88 +  4) y1
    pokeByteOff ptr (96 +  0) x1
    pokeByteOff ptr (96 +  4) y2
    pokeByteOff ptr (104+  0) x1
    pokeByteOff ptr (104+  4) y1
    pokeByteOff ptr (112+  0) x0
    pokeByteOff ptr (112+  4) y2
    pokeByteOff ptr (120+  0) x0
    pokeByteOff ptr (120+  4) y1

    pokeByteOff ptr (128+  0) x0
    pokeByteOff ptr (128+  4) y2
    pokeByteOff ptr (136+  0) x0
    pokeByteOff ptr (136+  4) y3
    pokeByteOff ptr (144+  0) x1
    pokeByteOff ptr (144+  4) y2
    pokeByteOff ptr (152+  0) x1
    pokeByteOff ptr (152+  4) y3
    pokeByteOff ptr (160+  0) x2
    pokeByteOff ptr (160+  4) y2
    pokeByteOff ptr (168+  0) x2
    pokeByteOff ptr (168+  4) y3
    pokeByteOff ptr (176+  0) x3
    pokeByteOff ptr (176+  4) y2
    pokeByteOff ptr (184+  0) x3
    pokeByteOff ptr (184+  4) y3



draw24 :: GUIData -> GUIState -> IO ()
draw24 gd gs = do
    glBindVertexArrayOES $ guidataVAO24 gd
    glDrawArrays gl_TRIANGLE_STRIP 0 24
    --glDrawArrays gl_LINE_STRIP 0 24
    

draw24ShapeSubSize :: GUIData -> GUIState -> GUIShape -> Float -> IO ()
draw24ShapeSubSize gd gs shape size = do
    writeVBO24ShapeSubSize gd gs (guidataVBO24 gd) shape size
    draw24 gd gs 


draw24ShapeAddSize :: GUIData -> GUIState -> GUIShape -> Float -> IO ()
draw24ShapeAddSize gd gs shape size = do
    writeVBO24ShapeAddSize gd gs (guidataVBO24 gd) shape size
    draw24 gd gs 








--------------------------------------------------------------------------------
--  VBO8

-- | assuming VBO of 8 * (2 * float).
writeVBO8ShapeSubSize :: GUIData -> GUIState -> GLuint -> GUIShape -> Float -> IO () 
writeVBO8ShapeSubSize gd gs vbo (GUIShape wth hth) size = do
    let x0 = 0.0 :: GLfloat
        x1 = rTF size :: GLfloat 
        x2 = rTF (wth - size) :: GLfloat
        x3 = rTF wth :: GLfloat 
        y0 = 0.0 :: GLfloat
        y1 = rTF hth :: GLfloat
    glBindBuffer gl_ARRAY_BUFFER vbo
    glBufferData gl_ARRAY_BUFFER 64 nullPtr gl_STREAM_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> 
        writeVBO8 x0 x1 x2 x3 y0 y1 ptr


-- | assuming vbo of 8 * (float float ushort ushort ushort ushort), with
--   stencilcoords and texcoords prewritten. corner size is 'size' units.
writeVBO8ShapeAddSize :: GUIData -> GUIState -> GLuint -> GUIShape -> Float -> IO () 
writeVBO8ShapeAddSize gd gs vbo (GUIShape wth hth) size = do
    let x0 = rTF (-size) :: GLfloat
        x1 = 0.0 :: GLfloat 
        x2 = rTF wth :: GLfloat
        x3 = rTF (wth + size) :: GLfloat 
        y0 = 0.0 :: GLfloat 
        y1 = rTF hth :: GLfloat
    glBindBuffer gl_ARRAY_BUFFER vbo
    glBufferData gl_ARRAY_BUFFER 64 nullPtr gl_STREAM_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> 
        writeVBO8 x0 x1 x2 x3 y0 y1 ptr


writeVBO8 :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> 
             GLfloat -> GLfloat -> Ptr a -> IO ()
writeVBO8 x0 x1 x2 x3 y0 y1 ptr = do
    pokeByteOff ptr (0  +  0) x0
    pokeByteOff ptr (0  +  4) y0
    pokeByteOff ptr (8  +  0) x0
    pokeByteOff ptr (8  +  4) y1
    pokeByteOff ptr (16 +  0) x1
    pokeByteOff ptr (16 +  4) y0
    pokeByteOff ptr (24 +  0) x1
    pokeByteOff ptr (24 +  4) y1
    pokeByteOff ptr (32 +  0) x2
    pokeByteOff ptr (32 +  4) y0
    pokeByteOff ptr (40 +  0) x2
    pokeByteOff ptr (40 +  4) y1
    pokeByteOff ptr (48 +  0) x3
    pokeByteOff ptr (48 +  4) y0
    pokeByteOff ptr (56 +  0) x3
    pokeByteOff ptr (56 +  4) y1



draw8 :: GUIData -> GUIState -> IO ()
draw8 gd gs = do
    glBindVertexArrayOES $ guidataVAO8 gd
    glDrawArrays gl_TRIANGLE_STRIP 0 8
    --glDrawArrays gl_LINE_STRIP 0 8
   

draw8ShapeSubSize :: GUIData -> GUIState -> GUIShape -> Float -> IO ()
draw8ShapeSubSize gd gs shape size = do
    writeVBO8ShapeSubSize gd gs (guidataVBO8 gd) shape size
    draw8 gd gs


draw8ShapeAddSize :: GUIData -> GUIState -> GUIShape -> Float -> IO ()
draw8ShapeAddSize gd gs shape size = do
    writeVBO8ShapeAddSize gd gs (guidataVBO8 gd) shape size
    draw8 gd gs



-- | subtracting squares horizontally
draw8ShapeSub :: GUIData -> GUIState -> GUIShape -> IO ()
draw8ShapeSub gd gs shape =
    draw8ShapeSubSize gd gs shape (shapeHth shape)

-- | adding squares horizontally
draw8ShapeAdd :: GUIData -> GUIState -> GUIShape -> IO ()
draw8ShapeAdd gd gs shape =
    draw8ShapeAddSize gd gs shape (shapeHth shape)






--------------------------------------------------------------------------------
--  VBO4

-- | write attPos. assuming vbo of 4 * (2* float + 2 * ushort)
writeVBO4Shape :: GUIData -> GUIState -> GLuint -> GUIShape -> IO () 
writeVBO4Shape gd gs vbo (GUIShape wth hth) = do
    let x0 = 0.0 :: GLfloat
        x1 = rTF wth :: GLfloat 
        y0 = 0.0 :: GLfloat
        y1 = rTF hth :: GLfloat 
    glBindBuffer gl_ARRAY_BUFFER vbo
    glBufferData gl_ARRAY_BUFFER 48 nullPtr gl_STREAM_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        pokeByteOff ptr (0  +  0) x0
        pokeByteOff ptr (0  +  4) y0
        pokeByteOff ptr (12 +  0) x0
        pokeByteOff ptr (12 +  4) y1
        pokeByteOff ptr (24 +  0) x1
        pokeByteOff ptr (24 +  4) y0
        pokeByteOff ptr (36 +  0) x1
        pokeByteOff ptr (36 +  4) y1



-- | write attPos + attTexCoord. assuming vbo of 4 * (2 * float + 2 * ushort)
writeVBO4ShapeTex :: GUIData -> GUIState -> GLuint -> GUIShape -> 
                     GLushort -> GLushort -> GLushort -> GLushort -> 
                     GLushort -> GLushort -> GLushort -> GLushort -> IO () 
writeVBO4ShapeTex gd gs vbo (GUIShape wth hth) s01 t01 s00 t00 s11 t11 s10 t10 = do
    let x0 = 0.0 :: GLfloat
        x1 = rTF wth :: GLfloat 
        y0 = 0.0 :: GLfloat
        y1 = rTF hth :: GLfloat 
    glBindBuffer gl_ARRAY_BUFFER vbo
    glBufferData gl_ARRAY_BUFFER 48 nullPtr gl_STREAM_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        pokeByteOff ptr (0  +  0) x0
        pokeByteOff ptr (0  +  4) y0
        pokeByteOff ptr (0  +  8) s01
        pokeByteOff ptr (0  + 10) t01
        pokeByteOff ptr (12 +  0) x0
        pokeByteOff ptr (12 +  4) y1
        pokeByteOff ptr (12 +  8) s00
        pokeByteOff ptr (12 + 10) t00
        pokeByteOff ptr (24 +  0) x1
        pokeByteOff ptr (24 +  4) y0
        pokeByteOff ptr (24 +  8) s11
        pokeByteOff ptr (24 + 10) t11
        pokeByteOff ptr (36 +  0) x1
        pokeByteOff ptr (36 +  4) y1
        pokeByteOff ptr (36 +  8) s10
        pokeByteOff ptr (36 + 10) t10





draw4 :: GUIData -> GUIState -> IO ()
draw4 gd gs = do
    glBindVertexArrayOES (guidataVAO4 gd)
    glDrawArrays gl_TRIANGLE_STRIP 0 4
    --glDrawArrays gl_LINE_STRIP 0 4


draw4Shape :: GUIData -> GUIState -> GUIShape -> IO ()
draw4Shape gd gs shape = do
    writeVBO4Shape gd gs (guidataVBO4 gd) shape
    draw4 gd gs


draw4ShapeTex :: GUIData -> GUIState -> GUIShape -> GLuint -> 
                 GLushort -> GLushort -> GLushort -> GLushort -> 
                 GLushort -> GLushort -> GLushort -> GLushort -> IO ()
draw4ShapeTex gd gs shape tex s01 t01 s00 t00 s11 t11 s10 t10 = do
    writeVBO4ShapeTex gd gs (guidataVBO4 gd) shape s01 t01 s00 t00 s11 t11 s10 t10
    draw4 gd gs




--------------------------------------------------------------------------------
--  Text

drawTexts :: GUIData -> GUIState -> [(GUIPos, String)] -> IO ()
drawTexts gd gs texts = do
    let GUIPos sx sy = guistatePos gs
        guish = guistateGUIShade gs
        fontsh = guidataFontShade gd
        fd = guidataFontData gd
    
    fontShade fontsh (guistateAlpha gs) (guistateProjModv gs)
    fontDrawDefault fontsh fd (guidataFontSize gd) (guidataFontColor gd)
    forM_ texts $ \(GUIPos x y, str) -> 
        fontDraw2D fontsh fd (sx + x) (sy + y) str

    -- reset GUIState
    glUseProgram $ guiShadePrg guish


drawCenteredTexts :: GUIData -> GUIState -> [(GUIPos, String)] -> IO ()
drawCenteredTexts gd gs texts = do
    let GUIPos sx sy = guistatePos gs
        guish = guistateGUIShade gs
        fontsh = guidataFontShade gd
        fd = guidataFontData gd
    
    fontShade fontsh (guistateAlpha gs) (guistateProjModv gs)
    fontDrawDefault fontsh fd (guidataFontSize gd) (guidataFontColor gd)
    forM_ texts $ \(GUIPos x y, str) -> 
        fontDraw2DCentered fontsh fd (sx + x) (sy + y) str

    -- reset GUIState
    glUseProgram $ guiShadePrg guish
