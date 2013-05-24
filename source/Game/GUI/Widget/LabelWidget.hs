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
module Game.GUI.Widget.LabelWidget
  (
    LabelWidget,

    makeLabelWidget,

    -- tmp:
    outputLabelWidget,

  ) where

import MyPrelude
import Game.GUI.Widget
import Game.GUI.Widget.Output



data LabelWidget a =
    LabelWidget
    {
        labelShape :: !GUIShape,
        labelString :: !String
    }


instance Widget LabelWidget where
    widgetShape = \gd gs w -> labelShape w
    widgetBegin = \gd gs w -> return w
    widgetEatInput = \gd gs w wi -> w
    widgetIterate = labelIterate



labelIterate :: GUIData -> GUIState -> LabelWidget a -> a -> IO (LabelWidget a, a)
labelIterate gd gs label a = do
    outputLabelWidget gd gs label
    return (label, a)


outputLabelWidget :: GUIData -> GUIState -> LabelWidget a -> IO ()
outputLabelWidget gd gs label = do
    useTexStencil gd gs 0 0 0
    draw4Shape gd gs (labelShape label)
    beginNoDepth gd gs
    drawTexts gd gs [(GUIPos 0.0 0.0, labelString label)]
    endNoDepth gd gs



--------------------------------------------------------------------------------
--  make


-- fixme: hth = 2.0 * unit (or guidataHth)

makeLabelWidget :: GUIData -> String -> LabelWidget a
makeLabelWidget gd str =
    let size = guidataFontSize gd
        charWth = fontdataCharWth (guidataFontData gd) size
        charHth = fontdataCharHth (guidataFontData gd) size
        wth = charWth * fI (length str)
        hth = charHth 
    in  LabelWidget
        {
            labelShape = GUIShape wth hth,
            labelString = str 
        }




