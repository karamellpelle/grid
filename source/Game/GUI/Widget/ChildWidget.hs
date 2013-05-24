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
module Game.GUI.Widget.ChildWidget
  (
    ChildWidget,

    makeChildWidget,

  ) where

import Game.GUI.Widget



data ChildWidget a =
    ChildWidget
    {
        childShape' :: GUIData -> GUIState -> GUIShape,
        childBegin' :: GUIData -> GUIState -> IO (ChildWidget a),
        childEatInput' :: GUIData -> GUIState -> WidgetInput -> ChildWidget a,
        childIterate' :: GUIData -> GUIState -> a -> IO (ChildWidget a, a)
    }


instance Widget ChildWidget where
    widgetShape = \gd gs w -> childShape' w gd gs
    widgetBegin = \gd gs w -> childBegin' w gd gs
    widgetEatInput = \gd gs w wi -> childEatInput' w gd gs wi
    widgetIterate = \gd gs w a -> childIterate' w gd gs a



--------------------------------------------------------------------------------
--  make

makeChildWidget :: Widget w => GUIData -> w a -> ChildWidget a
makeChildWidget _ w =
    ChildWidget
    {
        childShape' = \gd gs -> widgetShape gd gs w,

        childBegin' = \gd gs -> widgetBegin gd gs w >>= \w' -> 
                                    return $ makeChildWidget gd w',

        childEatInput' = \gd gs wi -> let w' = widgetEatInput gd gs w wi
                                      in  makeChildWidget gd w',
                                    
        childIterate' = \gd gs a -> widgetIterate gd gs w a >>= \(w', a') ->
                                    return (makeChildWidget gd w', a')
    }




