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
module Game.GUI.Widget.EmptyWidget
  (
    EmptyWidget,

    makeEmptyWidget,
    makeEmptyWidget',

  ) where

import MyPrelude
import Game.GUI.Widget



data EmptyWidget a =
    EmptyWidget


instance Widget EmptyWidget where
    widgetShape = \gd gs w -> GUIShape 0 0
    widgetBegin = \gd gs w -> return w
    widgetEatInput = \gd gs w wi -> w
    widgetIterate = \gd gs w a -> return (w, a)



makeEmptyWidget :: GUIData -> EmptyWidget a
makeEmptyWidget gd =
    makeEmptyWidget'


makeEmptyWidget' :: EmptyWidget a
makeEmptyWidget' =
    EmptyWidget




