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
module Game.GUI.Widget.TextWidget
(
  TextWidget,

  makeTextWidget,
  makeTextWidgetSplit,

  -- tmp:
  outputTextWidget,

) where


import MyPrelude
import Game.GUI.Widget
import Game.GUI.Widget.Output



data TextWidget a =
  TextWidget
  {
      -- textCentered :: !Bool,
      textShape :: !GUIShape,
      textLines :: ![String]
  }


instance Widget TextWidget where
  widgetShape = \gd gs w -> textShape w
  widgetBegin = \gd gs w -> return w
  widgetEatInput = \gd gs w wi -> w
  widgetIterate = textIterate



textIterate :: GUIData -> GUIState -> TextWidget a -> a -> IO (TextWidget a, a)
textIterate gd gs text a = do
  outputTextWidget gd gs text
  return (text, a)



outputTextWidget :: GUIData -> GUIState -> TextWidget a -> IO ()
outputTextWidget gd gs text = do
{-
  -- draw background
  gs' <- useFillTexMid gd gs
  useNoStencil gd gs'
  draw4ShapeNoTex gd gs' $ textShape text
-}
  beginNoDepth gd gs
  
  -- draw texts, stacked downwards
  drawTexts gd gs $ (flip zip) (textLines text) 
                  $ map (\y -> (GUIPos 0.0 y))
                  $ let dy = guidataFontSize gd
                    in  iterate (+ dy) 0.0
  
  endNoDepth gd gs


--------------------------------------------------------------------------------
--  make

-- fixme: hth = 2.0 * unit (or gamedataHth)
-- | make TextWidget from lines.
makeTextWidget :: GUIData -> [String] -> TextWidget a
makeTextWidget gd lines =
  let size = guidataFontSize gd
      charWth = fontdataCharWth (guidataFontData gd) size
      charHth = fontdataCharHth (guidataFontData gd) size
      wth = charWth * fI (foldl max 0 (map length lines))
      hth = charHth * fI (length lines)

  in  TextWidget
      {
          textShape = GUIShape wth hth,
          textLines = lines
      }

-- | make TextWidget from lines, which are "readably" splitted from 'str' 
--   at character width or newline.
makeTextWidgetSplit :: GUIData -> UInt -> String -> TextWidget a
makeTextWidgetSplit gd width str =
  makeTextWidget gd $ splitReadable width str


splitReadable :: UInt -> String -> [String]
splitReadable wth str = 
  concatMap (splitLine wth) $ lines str


splitLine :: UInt -> String -> [String]
splitLine linewth str =
  helper linewth "" $ myWords str
  where
    helper wth line [] = 
      case line of
          []    -> []
          _     -> [line]
    helper wth line (word:words) =
      let wordLen = fI $ length word
      in  if wordLen <= wth 
          -- add word to current line 
          then helper (wth - wordLen) (line ++ word) words
            -- add word to next line
            else case line of
                []    -> helper' wth word words
                line  -> line : helper' linewth word words

    helper' wth word words =
      case helper'' wth word of
          (0, ws, ws')  -> ws : helper' linewth ws' words
          (wth', ws, _) -> helper wth' ws words

    helper'' wth word =
      if wth == 0 
        then (wth, [], word)
        else case word of
            []      -> (wth, [], word)
            (w:ws)  -> let (wth', ws', ws'') = helper'' (wth - 1) ws
                       in  (wth', w:ws', ws'')

    myWords as =
        case as of
            []       -> []
            (a:as')  -> if myIsSpace a 
                        then [a] : myWords as'
                        else let (as', as'') = break myIsSpace as
                             in  as' : myWords as''

    myIsSpace ' ' = True
    myIsSpace _   = False


