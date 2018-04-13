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
module Game.GUI
  (
    guiIterate,
    
    module Game.GUI.Widget,
    module Game.GUI.GUIData,
    module Game.GUI.GUIShade,


  ) where


import MyPrelude
import Game.GUI.GUIData
import Game.GUI.GUIShade
import Game.GUI.Widget
import Data.List

import OpenGL
import OpenGL.Helpers


guiIterate :: Widget w => 
    GUIShade -> GUIData -> Mat4 -> Float -> Float -> Float -> 
    [WidgetInput] -> GUITick -> w a -> a -> IO (w a, a)
guiIterate sh gd projmodv alpha wth hth inputs tick = \w a -> do

    glUniform2f (guiShadeUniPos sh) 0.0 0.0
    glUniform2f (guiShadeUniScale sh) 1.0 1.0
    glUniform1f (guiShadeUniDepth sh) 0.0
    glUniform1f (guiShadeUniAlpha sh) (rTF alpha)
    glUniform1f (guiShadeUniFillTexRepeat sh) (rTF $ guidataFillTexRepeat gd)
    glUniform1f (guiShadeUniFocus sh) 0.0 -- fixme: implement in shader
    glUniform1i (guiShadeUniUseTex sh) (fI $ gl_FALSE)
    glUniform1i (guiShadeUniUseFillTex sh) (fI $ gl_FALSE)
    glUniform1i (guiShadeUniUseStencil sh) (fI $ gl_FALSE)

    let gs =  GUIState
              {
                  guistateTick = tick,
                  
                  guistateGUIShade = sh,

                  guistateProjModv = projmodv,
                  guistateAlpha = alpha,
                  guistateWth = wth,
                  guistateHth = hth,
                  
                  guistatePos = GUIPos 0.0 0.0,
                  guistateScaleX = 1.0,
                  guistateScaleY = 1.0,
                  guistateDepth = 0.0,
                  guistateFocus = 0.0,
                  guistateFillTex = 0
              }

    -- begin widget
    w' <- widgetBegin gd gs w

    -- eat input
    let w'' = foldl' (\w wi -> widgetEatInput gd gs w wi) w' inputs 

    -- output widget + step widget
    widgetIterate gd gs w'' a



