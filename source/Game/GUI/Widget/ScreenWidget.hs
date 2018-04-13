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
module Game.GUI.Widget.ScreenWidget
  (
    ScreenWidget,

    makeScreenWidget,

  ) where

import MyPrelude
import Game.GUI.Widget
import Game.GUI.Widget.ChildWidget
import Game.GUI.Widget.Output
import Game.GUI.Widget.Helpers



data ScreenWidget a =
    ScreenWidget
    {
        screenAlignX :: !Float,
        screenAlignY :: !Float,
        screenChild :: !(ChildWidget a)
    }


instance Widget ScreenWidget where
    widgetShape = screenShape
    widgetBegin = screenBegin
    widgetEatInput = screenEatInput
    widgetIterate = screenIterate



screenShape :: GUIData -> GUIState -> ScreenWidget a -> GUIShape
screenShape gd gs screen = 
    let wth = guistateWth gs
        hth = guistateHth gs
    in  GUIShape wth hth


screenBegin  :: GUIData -> GUIState -> ScreenWidget a -> IO (ScreenWidget a)
screenBegin gd gs screen = do
    child' <- widgetBegin gd gs $ screenChild screen
    return $ screen { screenChild = child' }


screenEatInput :: GUIData -> GUIState -> ScreenWidget a -> WidgetInput -> 
                  ScreenWidget a
screenEatInput gd gs screen wi = 
    let wth = guistateWth gs
        hth = guistateHth gs
        shape@(GUIShape wth' hth') = widgetShape gd gs $ screenChild screen
        x = screenAlignX screen * (wth - wth')
        y = screenAlignY screen * (hth - hth')
        child' = ifInputInsideThenElse gd gs (GUIPos x y) shape wi
                  (\gs' wi' -> widgetEatInput gd gs' (screenChild screen) wi')
                  (screenChild screen)

    in  screen { screenChild = child' }


screenIterate :: GUIData -> GUIState -> ScreenWidget a -> a -> IO (ScreenWidget a, a)
screenIterate gd gs screen a = do
    let wth = guistateWth gs
        hth = guistateHth gs
        GUIShape wth' hth' = widgetShape gd gs $ screenChild screen
        x = screenAlignX screen * (wth - wth')
        y = screenAlignY screen * (hth - hth')

    -- iterate child
    gs' <- plusPos gd gs $ GUIPos x y
    (child', a') <- widgetIterate gd gs' (screenChild screen) a
    -- fixme: reset pos??
    return (screen { screenChild = child' }, a')



--------------------------------------------------------------------------------
--  make


makeScreenWidget :: Widget w => GUIData -> Float -> Float -> w a -> ScreenWidget a
makeScreenWidget gd ax ay widget =
    ScreenWidget
    {
        screenAlignX = ax,
        screenAlignY = ay,
        screenChild = makeChildWidget gd widget
    }




