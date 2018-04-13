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
module Game.GUI.Widget
  (
    Widget (..),

    WidgetInput (..),
    WidgetInputType (..),

    module Game.GUI.GUIData,
    module Game.GUI.GUIState,

  ) where


import OpenGL
import Game.GUI.GUIData
import Game.GUI.GUIState


-- | note: currently, GUIState is only updated when widgetEatInput, widgetIterate
class Widget w where
    widgetShape :: GUIData -> GUIState -> w a -> GUIShape
    widgetBegin :: GUIData -> GUIState -> w a -> IO (w a)
    widgetEatInput :: GUIData -> GUIState -> w a -> WidgetInput -> w a 
    widgetIterate :: GUIData -> GUIState -> w a -> a -> IO (w a, a)


-- | WidgetInput's are always relative to vertex xy-space
data WidgetInput =
    WidgetInput
    {
        -- general data (actually pretty special :) )
        wiPos :: !GUIPos,
        wiPos' :: !GUIPos,
        wiTicks :: !GUITick,
        
        -- special data
        wiType :: !WidgetInputType
    }

data WidgetInputType =
    WIDrag    |
    WIDrop
    
-- note: WidgetInputs are (currently) only delivered to a widget if pos or pos' of
--       input is inside the widget. this is important for container widgets
--       (typically using ChildWidget's)



