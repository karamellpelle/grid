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
module Game.GUI.Widget.MaxWidget
  (
    MaxWidget,

    makeMaxWidget,

  ) where

import MyPrelude
import Game.GUI.Widget
import Game.GUI.Widget.ChildWidget
import Game.GUI.Widget.Helpers
import Game.GUI.Widget.Output




-- | MaxWidget has shape max(maxMaxShape, childShape)
data MaxWidget a =
    MaxWidget
    {
        maxChild :: !(ChildWidget a),
        maxMaxShape :: !GUIShape
    }




--------------------------------------------------------------------------------
--  Widget structure

instance Widget MaxWidget where
    widgetShape = maxShape
    widgetBegin = maxBegin
    widgetEatInput = maxEatInput
    widgetIterate = maxIterate


maxShape :: GUIData -> GUIState -> MaxWidget a -> GUIShape
maxShape gd gs max =
    let GUIShape wth hth = maxMaxShape max
        GUIShape wth' hth' = widgetShape gd gs $ maxChild max

    in  GUIShape (Prelude.max wth wth') (Prelude.max hth hth')


maxBegin :: GUIData -> GUIState -> MaxWidget a -> IO (MaxWidget a)
maxBegin gd gs max = do
    child' <- widgetBegin gd gs (maxChild max)
    return $ max { maxChild = child' }


maxEatInput :: GUIData -> GUIState -> MaxWidget a -> WidgetInput -> MaxWidget a
maxEatInput gd gs max wi =
    let shape@(GUIShape wth hth) = widgetShape gd gs $ maxChild max
        GUIShape wth' hth' = maxMaxShape max
        x = 0.5 * (Prelude.max wth wth' - wth)
        y = 0.5 * (Prelude.max hth hth' - hth)
        child' = ifInputInsideThenElse gd gs (GUIPos x y) shape wi
                 (\gs' wi' -> widgetEatInput gd gs' (maxChild max) wi')
                 (maxChild max)
    in  max { maxChild = child' }


maxIterate :: GUIData -> GUIState -> MaxWidget a -> a -> IO (MaxWidget a, a)
maxIterate gd gs max a = do
    let GUIShape wth hth = widgetShape gd gs $ maxChild max
        GUIShape wth' hth' = maxMaxShape max
        x = 0.5 * (Prelude.max wth wth' - wth)
        y = 0.5 * (Prelude.max hth hth' - hth)

    -- iterate child
    gs' <- plusPos gd gs $ GUIPos x y
    (child', a') <- widgetIterate gd gs' (maxChild max) a
    
    return (max { maxChild = child' }, a')




--------------------------------------------------------------------------------
--  make

makeMaxWidget :: Widget w => GUIData -> GUIShape -> w a -> MaxWidget a
makeMaxWidget gd shape w =
    MaxWidget
    {
        maxChild = makeChildWidget gd w,
        maxMaxShape = shape
    }


