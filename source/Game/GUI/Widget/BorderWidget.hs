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
module Game.GUI.Widget.BorderWidget
  (
    BorderWidget,

    makeBorderWidget,
    makeBorderWidgetSize,
  
  ) where



import MyPrelude
import Game.GUI.Widget
import Game.GUI.Widget.Output
import Game.GUI.Widget.Helpers
import Game.GUI.Widget.ChildWidget


data BorderWidget a =
    BorderWidget
    {
        borderChild :: !(ChildWidget a),
        borderSize :: !Float

    }



instance Widget BorderWidget where
    widgetShape = borderShape
    widgetBegin = borderBegin
    widgetEatInput = borderEatInput
    widgetIterate = borderIterate



--------------------------------------------------------------------------------
--  Widget structure


borderShape :: GUIData -> GUIState -> BorderWidget a -> GUIShape
borderShape gd gs = \border ->
    let size = borderSize border
        GUIShape wth hth = widgetShape gd gs (borderChild border)
    in  GUIShape (2.0 * size + wth) (2.0 * size + hth)


borderBegin :: GUIData -> GUIState -> BorderWidget a -> IO (BorderWidget a)
borderBegin gd gs border = do
    child' <- widgetBegin gd gs (borderChild border)
    return border { borderChild = child' }


borderEatInput :: GUIData -> GUIState -> BorderWidget a -> WidgetInput -> BorderWidget a
borderEatInput gd gs border wi =
    let gs' = gdgsPlusPos gd gs (GUIPos (borderSize border) (borderSize border))
    in  border { borderChild = widgetEatInput gd gs' (borderChild border) wi }


borderIterate :: GUIData -> GUIState -> BorderWidget a -> a -> IO (BorderWidget a, a)
borderIterate gd gs border a = do
   
    gs' <- plusPos gd gs (GUIPos (borderSize border) (borderSize border))

    -- draw FillTexBack before children
    gs'' <- useTexFillTexStencil gd gs' 0 (guidataFillTexBack gd) 
                                          (guidataBorderWidgetStencil gd) 0
    let shape = widgetShape gd gs'' (borderChild border)
    draw24ShapeAddSize gd gs'' shape (borderSize border)


    gs''' <- useFillTex gd gs'' (guidataFillTexMid gd)
      
    gs'''' <- incDepth gd gs'''

    -- iterate child
    (child', a') <- widgetIterate gd gs'''' (borderChild border) a

    resetDepth gd gs
    useTexStencil gd gs'''' 0 (guidataBorderWidgetStencil gd) 0
    draw24ShapeAddSize gd gs'''' shape (borderSize border)

    
    -- reset GUIState
    resetFillTex gd gs
    resetPos gd gs

    return (border { borderChild = child' }, a')
    
--------------------------------------------------------------------------------
--  make 


makeBorderWidgetSize :: Widget w => GUIData -> Float -> w a -> BorderWidget a
makeBorderWidgetSize gd size = \child -> 
    BorderWidget 
    {
        borderChild = makeChildWidget gd child,
        borderSize = size 
    }


makeBorderWidget :: Widget w => GUIData -> w a -> BorderWidget a
makeBorderWidget gd = \child -> 
    makeBorderWidgetSize gd size child
    where
      size = 0.02 -- ^ default size
