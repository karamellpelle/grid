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
module Game.GUI.Widget.MinWidget
  (
    MinWidget,

    makeMinWidget,

  ) where

import MyPrelude
import Game.GUI.Widget
import Game.GUI.Widget.ChildWidget
import Game.GUI.Widget.Helpers
import Game.GUI.Widget.Output

-- note: child draws outside minMinShape if greater shape


-- | MinWidget has shape min(minMinShape, childShape)
data MinWidget a =
    MinWidget
    {
        minChild :: !(ChildWidget a),
        minMinShape :: !GUIShape
    }




--------------------------------------------------------------------------------
--  Widget structure

instance Widget MinWidget where
    widgetShape = minShape
    widgetBegin = minBegin
    widgetEatInput = minEatInput
    widgetIterate = minIterate


minShape :: GUIData -> GUIState -> MinWidget a -> GUIShape
minShape gd gs min =
    let GUIShape wth hth = minMinShape min
        GUIShape wth' hth' = widgetShape gd gs $ minChild min

    in  GUIShape (Prelude.min wth wth') (Prelude.min hth hth')



minBegin :: GUIData -> GUIState -> MinWidget a -> IO (MinWidget a)
minBegin gd gs min = do
    child' <- widgetBegin gd gs (minChild min)
    return $ min { minChild = child' }


minEatInput :: GUIData -> GUIState -> MinWidget a -> WidgetInput -> MinWidget a
minEatInput gd gs min wi =
    let shape@(GUIShape wth hth) = widgetShape gd gs $ minChild min
        GUIShape wth' hth' = minMinShape min
        x = 0.5 * (Prelude.min wth wth' - wth)
        y = 0.5 * (Prelude.min hth hth' - hth)
        child' = ifInputInsideThenElse gd gs (GUIPos x y) shape wi
                 (\gs' wi' -> widgetEatInput gd gs' (minChild min) wi')
                 (minChild min)
    in  min { minChild = child' }


minIterate :: GUIData -> GUIState -> MinWidget a -> a -> IO (MinWidget a, a)
minIterate gd gs min a = do
    let GUIShape wth hth = widgetShape gd gs $ minChild min
        GUIShape wth' hth' = minMinShape min
        x = 0.5 * (Prelude.min wth wth' - wth)
        y = 0.5 * (Prelude.min hth hth' - hth)

    -- iterate child
    gs' <- plusPos gd gs $ GUIPos x y
    (child', a') <- widgetIterate gd gs' (minChild min) a
    
    return (min { minChild = child' }, a')




--------------------------------------------------------------------------------
--  make

makeMinWidget :: Widget w => GUIData -> GUIShape -> w a -> MinWidget a
makeMinWidget gd shape w =
    MinWidget
    {
        minChild = makeChildWidget gd w,
        minMinShape = shape
    }


