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
module Game.Font.FontShade
  (
    FontShade (..),

    loadFontShade,
    fontShade,
    fontShadePlane2D,
    fontShadePlane3D,
    fontSetAlpha,
    fontSetPlane2D,
    fontSetPlane3D,

    valueFontMaxCharacters,
  ) where


import MyPrelude
import File
import Game.Font.Buffer

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade


data FontShade =
    FontShade
    {
        fontShadePrg :: !GLuint,

        fontShadeUniAlpha :: !GLint,
        fontShadeUniProjModvMatrix :: !GLint,
        fontShadeUniPos :: !GLint,
        fontShadeUniPlaneX :: !GLint,
        fontShadeUniPlaneY :: !GLint,
        fontShadeUniTranslate :: !GLint,
        fontShadeUniCharSize :: !GLint,
        fontShadeUniColor :: !GLint,
        --fontShadeUniColorContour :: !GLint,


        fontShadeIBO :: !GLuint,
        fontShadeDefaultVAO :: !GLuint,
        fontShadeDefaultVBOPos :: !GLuint,
        fontShadeDefaultVBOCoord :: !GLuint

    }




--------------------------------------------------------------------------------
--  load

loadFontShade :: FilePath -> IO FontShade
loadFontShade path = do
    prg <- createPrg (path ++ "/FontShade.vsh") (path ++ "/FontShade.fsh") [  
                     (attPos, "a_pos"),
                     (attStencilCoord, "a_stencil_coord") ] [

                     (tex0, "u_stencil") ]

    -- uniforms
    uniAlpha <- getUniformLocation prg "u_alpha"
    uniProjModvMatrix <- getUniformLocation prg "u_projmodv_matrix"
    uniPos <- getUniformLocation prg "u_pos"
    uniPlaneX <- getUniformLocation prg "u_plane_x"
    uniPlaneY <- getUniformLocation prg "u_plane_y"
    uniColor <- getUniformLocation prg "u_color"
    --uniColorContour <- getUniformLocation prg "u_color_contour"
    uniCharSize <- getUniformLocation prg "u_char_size"
    uniTranslate <- getUniformLocation prg "u_translate"

    -- IBO for triangle strips
    ibo <- makeGroupIBO 4 valueFontMaxCharacters
    glBindBuffer gl_ELEMENT_ARRAY_BUFFER ibo

    -- Default
    (vao, vboPos, vboCoord) <- makeDefault ibo

    return $  FontShade
              {
                fontShadePrg = prg,

                fontShadeUniAlpha = uniAlpha,
                fontShadeUniProjModvMatrix = uniProjModvMatrix,
                fontShadeUniPos = uniPos,
                fontShadeUniPlaneX = uniPlaneX,
                fontShadeUniPlaneY = uniPlaneY,
                fontShadeUniTranslate = uniTranslate,
                fontShadeUniColor = uniColor,
                --fontShadeUniColorBack = uniColorBack,
                fontShadeUniCharSize = uniCharSize,
               
                fontShadeIBO = ibo,
                fontShadeDefaultVAO = vao,
                fontShadeDefaultVBOPos = vboPos,
                fontShadeDefaultVBOCoord = vboCoord
              }
  

makeDefault :: GLuint -> IO (GLuint, GLuint, GLuint)
makeDefault ibo = do
    -- vao 
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attStencilCoord

    -- bind ibo to vao
    glBindBuffer gl_ELEMENT_ARRAY_BUFFER ibo

    -- vbo pos
    vboPos <- bindNewBuf gl_ARRAY_BUFFER
    let bytesize = valueFontMaxCharacters * 16
    glBufferData gl_ARRAY_BUFFER (fI bytesize) nullPtr gl_STATIC_DRAW
    glVertexAttribPointer attPos 2 gl_UNSIGNED_SHORT gl_FALSE 0 $ 
                          mkPtrGLvoid 0
    writeBuf gl_ARRAY_BUFFER $ \ptr ->
        writeDefaultPosLen ptr valueFontMaxCharacters

    -- vbo coord
    vboCoord <- bindNewBuf gl_ARRAY_BUFFER
    let bytesize = valueFontMaxCharacters * 16
    glBufferData gl_ARRAY_BUFFER (fI bytesize) nullPtr gl_STREAM_DRAW
    glVertexAttribPointer attStencilCoord 2 gl_UNSIGNED_SHORT gl_TRUE 0 $ 
                          mkPtrGLvoid 0


    return (vao, vboPos, vboCoord)



--------------------------------------------------------------------------------
--  shade


-- | note: FrontFace is GL_CCW in 3D and GL_CW in 2D. 
--         so 2D drawing should typically disable GL_CULL_FACE!
fontShade :: FontShade -> Float -> Mat4 -> IO ()
fontShade sh alpha projmodv =
    fontShadePlane3D sh alpha projmodv 1.0 0.0 0.0 0.0 1.0 0.0


fontShadePlane2D :: FontShade -> Float -> Mat4 -> Float -> Float -> Float -> Float -> 
                    IO ()
fontShadePlane2D sh alpha projmodv x0 x1 y0 y1 =
    fontShadePlane3D sh alpha projmodv x0 x1 0.0 y0 y1 0.0


fontShadePlane3D :: FontShade -> Float -> Mat4 -> 
                    Float -> Float -> Float -> 
                    Float -> Float -> Float -> 
                    IO ()
fontShadePlane3D sh alpha projmodv x0 x1 x2 y0 y1 y2 = do
    glUseProgram (fontShadePrg sh)

    -- alpha
    glUniform1f (fontShadeUniAlpha sh) $ rTF alpha

    -- projmodv
    uniformMat4 (fontShadeUniProjModvMatrix sh) projmodv 

    -- plane
    glUniform3f (fontShadeUniPlaneX sh) (rTF x0) (rTF x1) (rTF x2)
    glUniform3f (fontShadeUniPlaneY sh) (rTF y0) (rTF y1) (rTF y2)



--------------------------------------------------------------------------------
--  configure shade


fontSetAlpha :: FontShade -> Float -> IO ()
fontSetAlpha sh alpha = 
    glUniform1f (fontShadeUniAlpha sh) $ rTF alpha


fontSetPlane2D :: FontShade -> Float -> Float -> Float -> Float -> IO () 
fontSetPlane2D sh x0 x1 y0 y1 =
    fontSetPlane3D sh x0 x1 0.0 y0 y1 0.0


fontSetPlane3D :: FontShade -> Float -> Float -> Float -> 
                               Float -> Float -> Float -> IO ()
fontSetPlane3D sh x0 x1 x2 y0 y1 y2 = do
    glUniform3f (fontShadeUniPlaneX sh) (rTF x0) (rTF x1) (rTF x2)
    glUniform3f (fontShadeUniPlaneY sh) (rTF y0) (rTF y1) (rTF y2)
    




--------------------------------------------------------------------------------
--  values

valueFontMaxCharacters :: UInt
valueFontMaxCharacters =
    64

