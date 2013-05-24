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
module Game.GUI.Widget.NumberWidget
  (
    NumberWidget,

    makeNumberWidget,
    onNewNumber,

    -- tmp:
    outputNumberWidget,

  ) where

import MyPrelude
import Game.GUI.Widget
import Game.GUI.Widget.Output
import Game.GUI.Widget.Helpers


data NumberWidget a =
    NumberWidget
    {
        numberShape :: !GUIShape,
        numberMin :: !Int,
        numberMax :: !Int,
        numberNumber :: !Int,
        numberIsDrag :: !Bool,
        numberDragTick :: !GUITick,
        numberIsNewNumber :: !Bool,
        numberActionNewNumber :: (Int -> a -> a)
    }


instance Widget NumberWidget where
    widgetShape = numberShape'
    widgetBegin = numberBegin
    widgetEatInput = numberEatInput
    widgetIterate = numberIterate


-- fixme: scale drag with guistateWth!

--------------------------------------------------------------------------------
--  


numberShape' :: GUIData -> GUIState -> NumberWidget a -> GUIShape
numberShape' gd gs number = 
    numberShape number


numberBegin :: GUIData -> GUIState -> NumberWidget a -> IO (NumberWidget a)
numberBegin gd gs number =
    return $ number { numberIsNewNumber = False }

    
numberEatInput :: GUIData -> GUIState -> NumberWidget a -> WidgetInput ->
                  NumberWidget a
numberEatInput gd gs number wi =
    case wiType wi of
        WIDrag    -> 
            dragNumber $ if wiIsTouched wi
                         then number 
                              {
                                  numberDragTick = wiTicks wi,
                                  numberIsDrag = True
                              }
                         else number
        WIDrop    -> 
            dropNumber number 
        -- _         -> number

    where
      dragNumber number =
          if numberIsDrag number 
              then let tick = numberDragTick number
                       tick' = wiTicks wi
                       xdrag = findXDrag wi
                   in if valueNumberRepeat <= (rTF tick' - rTF tick) * (xdrag * xdrag)
                      then let n' = keepInside (numberMin number) (numberMax number) $ 
                                    numberNumber number + floatSign xdrag
                           in  number
                               {
                                  numberNumber = n',
                                  numberDragTick = tick'
                               }
                      else number
              else number

      dropNumber number = 
          -- possible to change number with sweep 
          let n' = if numberDragTick number == 0.0
                   then keepInside (numberMin number) 
                                   (numberMax number) $ 
                                   numberNumber number + floatSign (findXDrag wi)
                   else numberNumber number
          in  number
              {
                numberNumber = n',
                numberIsDrag = False,
                numberIsNewNumber = True
              }

      findXDrag wi = 
          let GUIPos x _ = wiPos wi
              GUIPos x' _ = wiPos' wi
              xdrag = x' - x
          in  if valueXDragMin `absLEqual` xdrag
              then xdrag
              else 0.0

      absLEqual x y =
          x * x <= y * y

      floatSign x
          | x < 0.0 = -1
          | 0.0 < x = 1
          | otherwise = 0




numberIterate :: GUIData -> GUIState -> NumberWidget a -> a ->
                 IO (NumberWidget a, a)
numberIterate gd gs number a = do
    
    -- output
    outputNumberWidget gd gs number

    -- modify
    let a' = if numberIsNewNumber number 
             then let num = numberNumber number
                  in  numberActionNewNumber number num a
             else a

    return (number, a')


outputNumberWidget :: GUIData -> GUIState -> NumberWidget a -> IO ()
outputNumberWidget gd gs number = do
{- 
    -- draw background
    gs' <- useFillTexMid gd gs
    useNoStencil gd gs'
    draw4ShapeNoTex gd gs' $ numberShape number

-}
    -- draw number 
    case shapeScale 0.5 0.5 $ numberShape number of
        GUIShape wth hth  -> do
            drawCenteredTexts gd gs [(GUIPos wth hth, show (numberNumber number))]

    -- draw tex and fill
    gs' <- useTexFillTexStencil gd gs (guidataNumberWidgetTex gd) 0 0 0
    draw8ShapeSub gd gs' (numberShape number)
{-
    --beginNoDepth gd gs
    gs' <- useNoFillTex gd gs
    useTex gd gs' $ guidataNumberWidgetTex gd
    draw8ShapeSub gd gs' $ numberShape number
-}
    resetFillTex gd gs
    useTexStencil gd gs 0 (guidataNumberWidgetStencil gd) 0
    draw8 gd gs

{-
    setFillTex gd gs
    useNoTex gd gs
    useStencil gd gs $ guidataNumberWidgetStencil gd
    draw8 gd gs
-}
    --endNoDepth gd gs
    

--------------------------------------------------------------------------------
--  make


-- | make NumberWidget with numbers [min, max] (non-empty set)
makeNumberWidget :: GUIData -> Int -> Int -> Int -> NumberWidget a
makeNumberWidget gd nmin nmax n =
    let size = guidataFontSize gd
        (charWth, charHth) = fontdataCharSize (guidataFontData gd) size
        len = helper 10 1 $ if 0 <= nmin 
                            then nmax
                            else max nmax ((-nmin) + 1)
        shape = GUIShape (len * charWth + 2 * charHth) charHth

    in NumberWidget
       {
          numberShape = shape,
          numberMin = nmin,
          numberMax = nmax,
          numberNumber = n,
          numberIsDrag = False,
          numberDragTick = 0.0,
          numberIsNewNumber = False,
          numberActionNewNumber = \n a -> a
       }

    where
      -- find number of (digits + sign)
      helper a b x =
          if x < a then b
                   else helper (10 * a) (b + 1) x



--------------------------------------------------------------------------------
--  

-- | note: "new" does not mean n' != n
onNewNumber :: NumberWidget a -> (Int -> a -> a) -> NumberWidget a
onNewNumber number action =
    number { numberActionNewNumber = action }


--------------------------------------------------------------------------------
--  values

valueNumberRepeat :: Float
valueNumberRepeat = 
    0.005

valueXDragMin :: Float
valueXDragMin = 
    0.005
