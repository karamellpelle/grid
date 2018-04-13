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
module Game.GUI.Widget.ScaleWidget
  (
    ScaleWidget,

    makeScaleWidget,

  ) where


import MyPrelude
import Game.GUI.Widget
import Game.GUI.Widget.Output
import Game.GUI.Widget.ChildWidget





-- | ScaleWidget scales its child by specified amount.
data ScaleWidget a =
    ScaleWidget
    {
        scaleX :: Float,
        scaleY :: Float,
        scaleChild :: !(ChildWidget a)
    }


instance Widget ScaleWidget where
    widgetShape = scaleShape
    widgetBegin = scaleBegin
    widgetEatInput = scaleEatInput
    widgetIterate = scaleIterate



scaleShape :: GUIData -> GUIState -> ScaleWidget a -> GUIShape
scaleShape gd gs scale = 
    case widgetShape gd gs $ scaleChild scale of
        GUIShape wth hth -> GUIShape (wth * scaleX scale) (hth * scaleY scale)


scaleBegin :: GUIData -> GUIState -> ScaleWidget a -> IO (ScaleWidget a)
scaleBegin gd gs scale = do
    child' <- widgetBegin gd gs $ scaleChild scale
    return $ scale { scaleChild = child' }


scaleEatInput :: GUIData -> GUIState -> ScaleWidget a -> WidgetInput -> ScaleWidget a
scaleEatInput gd gs scale wi = 
    let gs' = gs { guistateScaleX = scaleX scale, guistateScaleY = scaleY scale }
        child' = widgetEatInput gd gs' (scaleChild scale) wi
    in  scale { scaleChild = child' }



scaleIterate :: GUIData -> GUIState -> ScaleWidget a -> a -> 
                IO (ScaleWidget a, a)
scaleIterate gd gs scale a = do
    gs' <- multScale gd gs (scaleX scale) (scaleY scale)
    (child', a') <- widgetIterate gd gs (scaleChild scale) a
    return (scale { scaleChild = child' }, a')



--------------------------------------------------------------------------------
--  make

makeScaleWidget :: Widget w => GUIData -> Float -> Float -> w a -> ScaleWidget a
makeScaleWidget gd ax ay w =
    ScaleWidget
    {
        scaleX = ax,
        scaleY = ay,
        scaleChild = makeChildWidget gd w
    }




