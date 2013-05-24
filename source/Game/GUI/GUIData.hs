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
module Game.GUI.GUIData
  (
    GUIData (..),
    GUIMake (..),

    makeGUIData,
    valuePressBTicks,


  ) where

import MyPrelude
import Game.Font

import System.Directory
import File

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade


data GUIData =
    GUIData
    {
        -- filltex's
        guidataFillTexBack :: !GLuint,
        guidataFillTexMid:: !GLuint,
        guidataFillTexFront :: !GLuint,

        guidataFillTexRepeat :: !GLfloat,

        -- vao/vbo
        guidataVAO24:: !GLuint,
        guidataVBO24 :: !GLuint,
        guidataVAO8 :: !GLuint,
        guidataVBO8 :: !GLuint,
        guidataVAO4 :: !GLuint,
        guidataVBO4 :: !GLuint,

        guidataFontShade :: FontShade,
        guidataFontData :: !FontData,
        guidataFontColor :: !FontColor,
        guidataFontSize :: !Float,

        -- special widget resources
        guidataBorderWidgetStencil :: !GLuint,
        guidataContourWidgetStencil :: !GLuint,
        guidataNumberWidgetTex :: !GLuint,
        guidataNumberWidgetStencil :: !GLuint,
        guidataSlideWidgetStencil :: !GLuint
    }


data GUIMake =
    GUIMake
    {
        mkGUIStyle :: !FilePath,
        mkGUIFillTexRepeat :: !GLfloat,

        mkGUIFontShade :: FontShade,
        mkGUIFontColor :: !FontColor,
        mkGUIFontSize :: !Float

    }

makeGUIData :: GUIMake -> IO GUIData
makeGUIData mk = do

    -- filltex's
    filltexBack <- makeFillTex $ mkGUIStyle mk  ++ "/filltex_back.png"
    filltexMid <- makeFillTex $ mkGUIStyle mk ++ "/filltex_mid.png"
    filltexFront <- makeFillTex $ mkGUIStyle mk ++ "/filltex_front.png"

    -- 24
    (vao24, vbo24) <- make24
    -- 8
    (vao8, vbo8) <- make8
    -- 4
    (vao4, vbo4) <- make4

    stencilBorderWidget <- makeStencilMaybe $ mkGUIStyle mk ++ "/BorderWidget_stencil.png"
    stencilContourWidget <- makeStencilMaybe $ mkGUIStyle mk ++ "/ContourWidget_stencil.png"
    texNumberWidget <- makeTexMaybe $ mkGUIStyle mk ++ "/NumberWidget_tex.png"
    stencilNumberWidget <- makeStencilMaybe $ mkGUIStyle mk ++ "/NumberWidget_stencil.png"
    stencilSlideWidget <- makeStencilMaybe $ mkGUIStyle mk ++ "/SlideWidget_stencil.png"

    fontdata <- makeFontData (mkGUIStyle mk ++ "/font") 16 8 0.45 0.80 32

    return $  GUIData
              {
                  guidataFillTexBack = filltexBack,
                  guidataFillTexMid = filltexMid,
                  guidataFillTexFront = filltexFront,
                  guidataFillTexRepeat = rTF $ mkGUIFillTexRepeat mk,
                  guidataVAO24 = vao24,
                  guidataVBO24 = vbo24,
                  guidataVAO8 = vao8,
                  guidataVBO8 = vbo8,
                  guidataVAO4 = vao4,
                  guidataVBO4 = vbo4,

                  guidataFontShade = mkGUIFontShade mk,
                  guidataFontData = fontdata,
                  guidataFontColor = mkGUIFontColor mk,
                  guidataFontSize = mkGUIFontSize mk,

                  guidataBorderWidgetStencil = stencilBorderWidget,
                  guidataContourWidgetStencil = stencilContourWidget,
                  guidataNumberWidgetTex = texNumberWidget,
                  guidataNumberWidgetStencil = stencilNumberWidget,
                  guidataSlideWidgetStencil = stencilSlideWidget
              }
    where
      makeStencil path = do 
          tex <- bindNewTex gl_TEXTURE_2D
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_CLAMP_TO_EDGE
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_CLAMP_TO_EDGE
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_LINEAR
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
          loadTexPreMult gl_TEXTURE_2D gl_RGBA path -- fixme: intfmt
          return tex

      makeFillTex path = do
          tex <- bindNewTex gl_TEXTURE_2D
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_REPEAT
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_REPEAT
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_NEAREST
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_NEAREST
          loadTexPreMult gl_TEXTURE_2D gl_RGBA path -- fixme: intfmt
          return tex
      
      makeTex = makeStencil

      makeStencilMaybe path = do
          doesFileExist path >>= \bool -> case bool of
              False   -> do
#ifdef DEBUG
                  putStrLn $ "GUI warning: could not load stencil " ++ path
#endif
                  return 0
              True    -> makeStencil path

      makeTexMaybe path = do
          doesFileExist path >>= \bool -> case bool of
              False   -> do
#ifdef DEBUG
                  putStrLn $ "GUI warning: could not load tex" ++ path
#endif
                  return 0

              True    -> makeTex path




--------------------------------------------------------------------------------
--  buffers

make24 :: IO (GLuint, GLuint)
make24 = do
    vao24 <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attTexCoord

    -- attPos
    vbo24 <- bindNewBuf gl_ARRAY_BUFFER
    glBufferData gl_ARRAY_BUFFER 192 nullPtr gl_STREAM_DRAW
    glVertexAttribPointer attPos 2 gl_FLOAT gl_FALSE 8 $ mkPtrGLvoid 0

    -- attTexCoord
    _ <- makeCoords
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_TRUE 4 $ mkPtrGLvoid 0

    return (vao24, vbo24 )
    
    where
      -- | 24 * (2 * ushort). 
      --   stencilcoords and texcoords written
      makeCoords = do
          let s0 = 0x0000 :: GLushort
              s1 = 0x8000 :: GLushort
              s2 = 0xffff :: GLushort
              t0 = 0x0000 :: GLushort
              t1 = 0x8000 :: GLushort
              t2 = 0xffff :: GLushort
          vbo <- bindNewBuf gl_ARRAY_BUFFER
          glBufferData gl_ARRAY_BUFFER 96 nullPtr gl_STATIC_DRAW
          writeBuf gl_ARRAY_BUFFER $ \ptr -> do
              pokeByteOff ptr (0  +  0) s0
              pokeByteOff ptr (0  +  2) t2
              pokeByteOff ptr (4  +  0) s0
              pokeByteOff ptr (4  +  2) t1
              pokeByteOff ptr (8  +  0) s1
              pokeByteOff ptr (8  +  2) t2
              pokeByteOff ptr (12 +  0) s1
              pokeByteOff ptr (12 +  2) t1
              pokeByteOff ptr (16 +  0) s1
              pokeByteOff ptr (16 +  2) t2
              pokeByteOff ptr (20 +  0) s1
              pokeByteOff ptr (20 +  2) t1
              pokeByteOff ptr (24 +  0) s2
              pokeByteOff ptr (24 +  2) t2
              pokeByteOff ptr (28 +  0) s2
              pokeByteOff ptr (28 +  2) t1

              pokeByteOff ptr (32 +  0) s0
              pokeByteOff ptr (32 +  2) t1
              pokeByteOff ptr (36 +  0) s0
              pokeByteOff ptr (36 +  2) t1
              pokeByteOff ptr (40 +  0) s1
              pokeByteOff ptr (40 +  2) t1
              pokeByteOff ptr (44 +  0) s1
              pokeByteOff ptr (44 +  2) t1
              pokeByteOff ptr (48 +  0) s1
              pokeByteOff ptr (48 +  2) t1
              pokeByteOff ptr (52 +  0) s1
              pokeByteOff ptr (52 +  2) t1
              pokeByteOff ptr (56 +  0) s2
              pokeByteOff ptr (56 +  2) t1
              pokeByteOff ptr (60 +  0) s2
              pokeByteOff ptr (60 +  2) t1

              pokeByteOff ptr (64 +  0) s0
              pokeByteOff ptr (64 +  2) t1
              pokeByteOff ptr (68 +  0) s0
              pokeByteOff ptr (68 +  2) t0
              pokeByteOff ptr (72 +  0) s1
              pokeByteOff ptr (72 +  2) t1
              pokeByteOff ptr (76 +  0) s1
              pokeByteOff ptr (76 +  2) t0
              pokeByteOff ptr (80 +  0) s1
              pokeByteOff ptr (80 +  2) t1
              pokeByteOff ptr (84 +  0) s1
              pokeByteOff ptr (84 +  2) t0
              pokeByteOff ptr (88 +  0) s2
              pokeByteOff ptr (88 +  2) t1
              pokeByteOff ptr (92 +  0) s2
              pokeByteOff ptr (92 +  2) t0

          return vbo



make8 :: IO (GLuint, GLuint)
make8 = do
    vao8 <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attTexCoord

    -- attPos
    vbo8 <- bindNewBuf gl_ARRAY_BUFFER
    glBufferData gl_ARRAY_BUFFER 64 nullPtr gl_STREAM_DRAW
    glVertexAttribPointer attPos 2 gl_FLOAT gl_FALSE 8 $ mkPtrGLvoid 0

    -- attTexCoord
    _ <- makeCoords
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_TRUE 4 $ mkPtrGLvoid 0
    
    return (vao8, vbo8)
    
    where
      -- | 8 * (2 * ushort). 
      makeCoords = do
          let s0 = 0x0000 :: GLushort
              s1 = 0x8000 :: GLushort
              s2 = 0xffff :: GLushort
              t0 = 0x0000 :: GLushort
              t1 = 0xffff :: GLushort
          vbo <- bindNewBuf gl_ARRAY_BUFFER
          glBufferData gl_ARRAY_BUFFER 32 nullPtr gl_STATIC_DRAW
          writeBuf gl_ARRAY_BUFFER $ \ptr -> do
              pokeByteOff ptr (0  +  0) s0
              pokeByteOff ptr (0  +  2) t1
              pokeByteOff ptr (4  +  0) s0
              pokeByteOff ptr (4  +  2) t0
              pokeByteOff ptr (8  +  0) s1
              pokeByteOff ptr (8  +  2) t1
              pokeByteOff ptr (12 +  0) s1
              pokeByteOff ptr (12 +  2) t0
              pokeByteOff ptr (16 +  0) s1
              pokeByteOff ptr (16 +  2) t1
              pokeByteOff ptr (20 +  0) s1
              pokeByteOff ptr (20 +  2) t0
              pokeByteOff ptr (24 +  0) s2
              pokeByteOff ptr (24 +  2) t1
              pokeByteOff ptr (28 +  0) s2
              pokeByteOff ptr (28 +  2) t0

          return vbo


make4 :: IO (GLuint, GLuint)
make4 = do
    vao4 <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attTexCoord

    -- attPos
    vbo4 <- bindNewBuf gl_ARRAY_BUFFER
    glBufferData gl_ARRAY_BUFFER 48 nullPtr gl_STREAM_DRAW
    glVertexAttribPointer attPos 2 gl_FLOAT gl_FALSE 12 $ mkPtrGLvoid 0
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_TRUE 12 $ mkPtrGLvoid 8
    

    return (vao4, vbo4)
    


--------------------------------------------------------------------------------
--  values

valuePressBTicks :: Double
valuePressBTicks = 1.0

