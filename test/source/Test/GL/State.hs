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
module Test.GL.State
  (
    TestGLState (..),
    makeTestGLState,

  ) where

import MyPrelude
import Game
import Game.Grid

import OpenGL
import OpenGL.Helpers

import Game.GUI
import Game.GUI.Widget.ScreenWidget
import Game.GUI.Widget.BorderWidget
import Game.GUI.Widget.TextWidget


type WidgetT =
    ScreenWidget ()


data TestGLState =
    TestGLState
    {
        tglstateWidget :: WidgetT,
        tglstatePath :: Path
    }


makeTestGLState :: MEnv' TestGLState
makeTestGLState = do
    gd <- resourceGUIData
    path <- makePath 1
    return TestGLState
           {
              tglstateWidget = makeWidget gd,
              tglstatePath = path { pathAlpha = 0.5 }
           }
    where
      makeWidget gd = 
          makeScreenWidget gd 0.5 0.5 $ 
          makeBorderWidgetSize gd 0.05 $ 
          makeTextWidget gd ["haskell", "Text", "Widget" ]
