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
module Game.GUI.Widget.Output
  (
    plusPos,
    plusPos',
    resetPos,
    multScale,
    resetScale,
    focus,
    resetFocus,
    beginCut,
    insideCut,
    endCut,
    incDepth,
    resetDepth,
    beginNoDepth,
    endNoDepth,
    useTexFillTexStencil,
    useTexStencil,
    useTex,
    useFillTex,
    useStencil,
    useNoTex,
    useNoFillTex,
    useNoStencil,
    resetFillTex,
    stencilDim,

    module Game.GUI.Widget.Output.Draw,

  ) where


import MyPrelude

import Game.GUI.GUIData
import Game.GUI.GUIShade
import Game.GUI.Widget
import Game.GUI.Widget.Helpers
import Game.GUI.Widget.Output.Draw

import OpenGL
import OpenGL.Helpers



--------------------------------------------------------------------------------
--  

-- this is not used, since each widget is responsible to set state back to previous!
-- (except fill tex)
{-
setGUIState :: GUIData -> GUIState -> IO ()
setGUIState gd gs = do
    let sh = guidataShadeGUI gd
        GUIPos x y = guistatePos gs
        ax = guistateScaleX gs
        ay = guistateScaleY gs
        depth = guistateDepth gs
        filltex = guistateFillTex gs
        tweak = guistateTweak gs
        alpha = guistateAlpha gs
    glUniform2f (shadeGUIUniPos sh) (rTF x) (rTF y)
    glUniform2f (shadeGUIUniScale sh) (rTF ax) (rTF ay)
    glUniform1f (shadeGUIUniDepth sh) (rTF depth)
    glUniform1f (shadeGUIUniTweak sh) (rTF tweak)
    glUniform1f (shadeGUIUniAlpha sh) (rTF alpha)
    case filltex of
        0       -> glUniform1i (shadeGUIUniUseFillTex sh) (fI gl_FALSE)
        filltex -> do
            glUniform1i (shadeGUIUniUseFillTex sh) (fI gl_TRUE)
            glActiveTexture gl_TEXTURE0
            glBindTexture gl_TEXTURE_2D filltex
-}          



--------------------------------------------------------------------------------
--  pos

plusPos :: GUIData -> GUIState -> GUIPos -> IO GUIState
plusPos gd gs pos = do
    let pos' = posScalePlus (guistatePos gs) (guistateScaleX gs) (guistateScaleY gs)
                            pos
    glUniform2f (guiShadeUniPos $ guistateGUIShade gs) 
                (rTF $ posX pos') (rTF $ posY pos')
    return gs { guistatePos = pos' }


-- | plus pos without current scaling
plusPos' :: GUIData -> GUIState -> GUIPos -> IO GUIState
plusPos' gd gs pos = do
    let pos' = posPlus (guistatePos gs) pos
    glUniform2f (guiShadeUniPos $ guistateGUIShade gs)
                (rTF $ posX pos') (rTF $ posY pos')
    return gs { guistatePos = pos' }


resetPos :: GUIData -> GUIState -> IO ()
resetPos gd gs =
    glUniform2f (guiShadeUniPos $ guistateGUIShade gs)
                (rTF $ posX $ guistatePos gs)
                (rTF $ posY $ guistatePos gs)


--------------------------------------------------------------------------------
--  scale

multScale :: GUIData -> GUIState -> Float -> Float -> IO GUIState
multScale gd gs ax ay = do
    let ax' = guistateScaleX gs * ax
        ay' = guistateScaleY gs * ay
    glUniform2f (guiShadeUniScale $ guistateGUIShade gs)
                (rTF ax') (rTF ay')
    return  gs {
                  guistateScaleX = ax',
                  guistateScaleY = ay'
               }

resetScale :: GUIData -> GUIState -> IO ()
resetScale gd gs = 
    glUniform2f (guiShadeUniScale $ guistateGUIShade gs)
                (rTF $ guistateScaleX gs)
                (rTF $ guistateScaleY gs)



--------------------------------------------------------------------------------
--  focus

focus :: GUIData -> GUIState -> Float -> IO GUIState
focus gd gs focus = do
    let focus' = guistateFocus gs - guistateFocus gs * focus + focus
    glUniform1f (guiShadeUniFocus $ guistateGUIShade gs) 
                (rTF focus')
    return  gs { guistateFocus = focus' }


resetFocus :: GUIData -> GUIState -> IO ()
resetFocus gd gs = do
    glUniform1f (guiShadeUniFocus $ guistateGUIShade gs) 
                (rTF $ guistateFocus gs)


--------------------------------------------------------------------------------
--  cut

beginCut :: GUIData -> GUIState -> IO () 
beginCut gd gs = do
    glEnable gl_STENCIL_TEST
    glStencilFunc gl_ALWAYS 0x01 0xff
    glStencilOp gl_KEEP gl_KEEP gl_REPLACE


insideCut :: GUIData -> GUIState -> IO ()
insideCut gd gs = do
    glStencilFunc gl_EQUAL 0x01 0xff


endCut :: GUIData -> GUIState -> IO ()
endCut gd gs = do
    glDisable gl_STENCIL_TEST


--------------------------------------------------------------------------------
--  depth

incDepth :: GUIData -> GUIState -> IO GUIState
incDepth gd gs = do
    let depth' = guistateDepth gs + epsilon
    glUniform1f (guiShadeUniDepth $ guistateGUIShade gs)
                (rTF depth')
    return  gs { guistateDepth = depth' }
    where
      -- this needs to be low for clipping, and also if we want to draw 
      -- GUI in 3D:
      epsilon = 0.01 


resetDepth :: GUIData -> GUIState -> IO ()
resetDepth gd gs = do
    glUniform1f (guiShadeUniDepth $ guistateGUIShade gs)  
                (rTF $ guistateDepth gs)


beginNoDepth :: GUIData -> GUIState -> IO ()
beginNoDepth gd gs = do
    glDepthMask gl_FALSE
    glDepthFunc gl_ALWAYS


endNoDepth :: GUIData -> GUIState -> IO ()
endNoDepth gd gs = do
    glDepthFunc gl_LEQUAL
    glDepthMask gl_TRUE



--------------------------------------------------------------------------------
--  textures

useTexFillTexStencil :: GUIData -> GUIState -> GLuint -> GLuint -> GLuint -> 
                        UInt -> IO GUIState
useTexFillTexStencil gd gs tex filltex stencil stencildim = do
    if tex == 0 then glUniform1i (guiShadeUniUseTex $ guistateGUIShade gs) $ 
                     fI gl_FALSE
                else do
                  glUniform1i (guiShadeUniUseTex $ guistateGUIShade gs) $ 
                              fI gl_TRUE
                  glActiveTexture gl_TEXTURE0 
                  glBindTexture gl_TEXTURE_2D tex
    if filltex == 0 then glUniform1i (guiShadeUniUseFillTex $ guistateGUIShade gs) $ 
                         fI gl_FALSE
                    else do
                      glUniform1i (guiShadeUniUseFillTex $ guistateGUIShade gs) $ 
                                  fI gl_TRUE
                      glActiveTexture gl_TEXTURE1
                      glBindTexture gl_TEXTURE_2D filltex
    if stencil == 0 then glUniform1i (guiShadeUniUseStencil $ guistateGUIShade gs) $ 
                         fI gl_FALSE
                    else do
                      glUniform1i (guiShadeUniUseStencil $ guistateGUIShade gs) $ 
                                  fI gl_TRUE
                      glActiveTexture gl_TEXTURE2
                      glBindTexture gl_TEXTURE_2D stencil
                      case stencildim of
                          0   -> glUniform3f (guiShadeUniStencilDim $ guistateGUIShade gs)
                                             1.0 0.0 0.0
                          1   -> glUniform3f (guiShadeUniStencilDim $ guistateGUIShade gs)
                                             0.0 1.0 0.0
                          2   -> glUniform3f (guiShadeUniStencilDim $ guistateGUIShade gs)
                                             0.0 0.0 1.0
                          _   -> do
#ifdef DEBUG
                                 putStrLn "GUI warning: invalid stencil dim"
#endif
                                 return ()


    return gs { guistateFillTex = filltex } 


useTexStencil :: GUIData -> GUIState -> GLuint -> GLuint -> UInt -> IO ()
useTexStencil gd gs tex stencil stencildim = do
    if tex == 0 then glUniform1i (guiShadeUniUseTex $ guistateGUIShade gs) $ 
                     fI gl_FALSE
                else do
                  glUniform1i (guiShadeUniUseTex $ guistateGUIShade gs) $ 
                              fI gl_TRUE
                  glActiveTexture gl_TEXTURE0 
                  glBindTexture gl_TEXTURE_2D tex
    if stencil == 0 then glUniform1i (guiShadeUniUseStencil $ guistateGUIShade gs) $ 
                         fI gl_FALSE
                    else do
                      glUniform1i (guiShadeUniUseStencil $ guistateGUIShade gs) $ 
                                  fI gl_TRUE
                      glActiveTexture gl_TEXTURE2
                      glBindTexture gl_TEXTURE_2D stencil
                      case stencildim of
                          0   -> glUniform3f (guiShadeUniStencilDim $ guistateGUIShade gs)
                                             1.0 0.0 0.0
                          1   -> glUniform3f (guiShadeUniStencilDim $ guistateGUIShade gs)
                                             0.0 1.0 0.0
                          2   -> glUniform3f (guiShadeUniStencilDim $ guistateGUIShade gs)
                                             0.0 0.0 1.0
                          _   -> do
#ifdef DEBUG
                                 putStrLn "GUI warning: invalid stencil dim"
#endif
                                 return ()



useTex :: GUIData -> GUIState -> GLuint -> IO ()
useTex gd gs tex = do
    glUniform1i (guiShadeUniUseTex $ guistateGUIShade gs) $ fI gl_TRUE
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D tex


useFillTex :: GUIData -> GUIState -> GLuint -> IO GUIState
useFillTex gd gs filltex = do
    glUniform1i (guiShadeUniUseFillTex $ guistateGUIShade gs) $ fI gl_TRUE
    glActiveTexture gl_TEXTURE1
    glBindTexture gl_TEXTURE_2D filltex
    return gs { guistateFillTex = filltex }


useStencil :: GUIData -> GUIState -> GLuint -> UInt -> IO ()
useStencil gd gs stencil dim = do 
    glUniform1i (guiShadeUniUseStencil $ guistateGUIShade gs) $ fI gl_TRUE
    case dim of
        0   -> glUniform3f (guiShadeUniStencilDim $ guistateGUIShade gs) 1.0 0.0 0.0
        1   -> glUniform3f (guiShadeUniStencilDim $ guistateGUIShade gs) 0.0 1.0 0.0
        2   -> glUniform3f (guiShadeUniStencilDim $ guistateGUIShade gs) 0.0 0.0 1.0
        _   -> do
#ifdef DEBUG
               putStrLn "GUI warning: invalid stencil dim"
#endif
               return ()
    glActiveTexture gl_TEXTURE2
    glBindTexture gl_TEXTURE_2D stencil


stencilDim :: GUIData -> GUIState -> UInt -> IO ()
stencilDim gd gs dim = 
    case dim of
        0   -> glUniform3f (guiShadeUniStencilDim $ guistateGUIShade gs) 1.0 0.0 0.0
        1   -> glUniform3f (guiShadeUniStencilDim $ guistateGUIShade gs) 0.0 1.0 0.0
        2   -> glUniform3f (guiShadeUniStencilDim $ guistateGUIShade gs) 0.0 0.0 1.0
        _   -> do
#ifdef DEBUG
               putStrLn "GUI warning: invalid stencil dim"
#endif
               return ()
      

useNoTex :: GUIData -> GUIState -> IO ()
useNoTex gd gs = do
    glUniform1i (guiShadeUniUseTex $ guistateGUIShade gs) $ fI gl_FALSE

useNoFillTex :: GUIData -> GUIState -> IO GUIState
useNoFillTex gd gs = do
    glUniform1i (guiShadeUniUseFillTex $ guistateGUIShade gs) $ fI gl_FALSE
    return  gs { guistateFillTex = 0 }

useNoStencil :: GUIData -> GUIState -> IO ()
useNoStencil gd gs = do
    glUniform1i (guiShadeUniUseStencil $ guistateGUIShade gs) $ fI gl_FALSE


resetFillTex :: GUIData -> GUIState -> IO ()
resetFillTex gd gs = do
    case guistateFillTex gs of
        0         -> do
            glUniform1i (guiShadeUniUseFillTex $ guistateGUIShade gs) $ fI gl_FALSE

        filltex   -> do
            glUniform1i (guiShadeUniUseFillTex $ guistateGUIShade gs) $ fI gl_TRUE
            glActiveTexture gl_TEXTURE1
            glBindTexture gl_TEXTURE_2D filltex


