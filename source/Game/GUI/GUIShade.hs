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
module Game.GUI.GUIShade
  (
    GUIShade (..),
    loadGUIShade,
    
    guiShade,

  ) where

import MyPrelude
import OpenGL
import OpenGL.Helpers
import OpenGL.Shade


data GUIShade =
    GUIShade
    {
        guiShadePrg :: !GLuint,
        guiShadeUniProjModvMatrix :: !GLint,
        guiShadeUniAlpha :: !GLint,

        guiShadeUniPos :: !GLint,
        guiShadeUniScale :: !GLint,
        guiShadeUniDepth :: !GLint,
        guiShadeUniFillTexRepeat :: !GLint,
        guiShadeUniFocus :: !GLint,
        
        guiShadeUniUseFillTex :: !GLint,
        guiShadeUniUseTex :: !GLint,
        guiShadeUniUseStencil :: !GLint,

        guiShadeUniStencilDim :: !GLint

    }


loadGUIShade :: FilePath -> IO GUIShade
loadGUIShade path = do
    prg <- createPrg (path ++ "/GUIShade.vsh") (path ++ "/GUIShade.fsh") [
                     (attPos, "a_pos"),
                     (attTexCoord, "a_tex_coord") ] [

                     (tex0, "u_tex"),
                     (tex1, "u_filltex"),
                     (tex2, "u_stencil") ]

    uProjModvMatrix <- getUniformLocation prg "u_projmodv_matrix"
    uAlpha <- getUniformLocation prg "u_alpha"
    uPos <- getUniformLocation prg "u_pos"
    uScale <- getUniformLocation prg "u_scale"
    uDepth <- getUniformLocation prg "u_depth"
    uFillTexRepeat <- getUniformLocation prg "u_filltex_repeat"
    uFocus <- getUniformLocation prg "u_focus"
    uUseFillTex <- getUniformLocation prg "u_use_filltex"
    uUseTex <- getUniformLocation prg "u_use_tex"
    uUseStencil <- getUniformLocation prg "u_use_stencil"
    uStencilDim <- getUniformLocation prg "u_stencil_dim"
    
    return  GUIShade
            {
                guiShadePrg = prg,
                guiShadeUniProjModvMatrix = uProjModvMatrix,
                guiShadeUniAlpha = uAlpha,

                guiShadeUniPos = uPos,
                guiShadeUniScale = uScale,
                guiShadeUniDepth = uDepth,
                guiShadeUniFillTexRepeat = uFillTexRepeat,
                guiShadeUniFocus = uFocus,
                
                guiShadeUniUseFillTex = uUseFillTex,
                guiShadeUniUseTex = uUseTex,
                guiShadeUniUseStencil = uUseStencil,

                guiShadeUniStencilDim = uStencilDim
            }
    


-- | 2D shading for GUI. y direction is top to bottom (read direction). hence
--   we use a left-handed coordinate system. 
--   this enables gl_DEPTH_TEST and disables gl_CULL_FACE
--   (FrontFace = CCW, CullFace disabled)
guiShade :: GUIShade -> Float -> Float -> Float -> IO Mat4
guiShade sh alpha wth hth = do

    glEnable gl_DEPTH_TEST
    glDisable gl_CULL_FACE

    glUseProgram $ guiShadePrg sh

    -- projection modelview
    let projmodv = mat4Ortho 0 wth hth 0 (-1) 1
    uniformMat4 (guiShadeUniProjModvMatrix sh) projmodv

    -- alpha
    glUniform1f (guiShadeUniAlpha sh) $ rTF alpha -- fixme: remove into guiIterate?

    return projmodv



