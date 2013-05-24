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
module Game.GUI.Widget.ContourWidget
(
  ContourWidget,

  makeContourWidget,
  makeContourWidgetSize,

) where


import MyPrelude
import Game.GUI.Widget
import Game.GUI.Widget.Output
import Game.GUI.Widget.ChildWidget

import OpenGL
import OpenGL.Helpers


data ContourWidget a =
  ContourWidget
  {
      contourChild :: !(ChildWidget a),
      contourSize :: !Float
  }


--  Widget structure
instance Widget ContourWidget where
  widgetShape = \gd gs w -> widgetShape gd gs (contourChild w)
  widgetBegin = contourBegin
  widgetEatInput = contourEatInput
  widgetIterate = contourIterate






contourBegin :: GUIData -> GUIState -> ContourWidget a -> IO (ContourWidget a)
contourBegin gd gs contour = 
    widgetBegin gd gs (contourChild contour) >>= \child' ->
        return contour { contourChild = child' }

contourEatInput :: GUIData -> GUIState -> ContourWidget a -> WidgetInput -> 
                   ContourWidget a
contourEatInput gd gs contour wi =
    let child' = widgetEatInput gd gs (contourChild contour) wi
    in  contour { contourChild = child' }


contourIterate :: GUIData -> GUIState -> ContourWidget a -> a -> 
                  IO (ContourWidget a, a)
contourIterate gd gs contour a = do
    -- iterate child
    (child', a') <- widgetIterate gd gs (contourChild contour) a


    -- draw contour 
    gs' <- incDepth gd gs
    useTexStencil gd gs' 0 (guidataContourWidgetStencil gd) 0
    glDepthMask gl_FALSE
    draw24ShapeSubSize gd gs (widgetShape gd gs contour) (contourSize contour) 
    glDepthMask gl_TRUE

    -- reset
    resetDepth gd gs


    return (contour { contourChild = child' }, a')


--------------------------------------------------------------------------------
--  make

makeContourWidgetSize :: Widget w => GUIData -> Float -> w a -> ContourWidget a
makeContourWidgetSize gd size = \w -> 
    ContourWidget
    {
        contourChild = makeChildWidget gd w,
        contourSize = size
    }


makeContourWidget :: Widget w => GUIData -> w a -> ContourWidget a
makeContourWidget gd = \w -> 
    makeContourWidgetSize gd size w
    where
        size = 0.08








