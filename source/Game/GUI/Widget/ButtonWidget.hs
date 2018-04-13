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
module Game.GUI.Widget.ButtonWidget
  (
    ButtonWidget,

    onPressA,
    onPressB,
    
    makeButtonWidget,

  ) where


import MyPrelude
import Game.GUI.Widget
import Game.GUI.Widget.Output
import Game.GUI.Widget.Helpers
import Game.GUI.Widget.ChildWidget



data ButtonWidget a =
    ButtonWidget
    {
        buttonChild :: !(ChildWidget a),

        buttonOK :: !Bool,
        buttonNotOKTick :: !GUITick,
        buttonIsPressA :: !Bool,
        buttonIsPressB :: !Bool,

        buttonActionPressA :: a -> a,
        buttonActionPressB :: a -> a

    }


--------------------------------------------------------------------------------
--  Widget structure


instance Widget ButtonWidget where
    widgetShape = buttonShape
    widgetBegin = buttonBegin
    widgetEatInput = buttonEatInput
    widgetIterate = buttonIterate


buttonShape :: GUIData -> GUIState -> ButtonWidget a -> GUIShape
buttonShape gd gs button = 
    widgetShape gd gs $ buttonChild button



buttonBegin :: GUIData -> GUIState -> ButtonWidget a -> 
               IO (ButtonWidget a)
buttonBegin gd gs button = 
    widgetBegin gd gs (buttonChild button) >>= \child' ->
        return $  button
                  {
                    buttonChild = child',
                    buttonIsPressA = False,
                    buttonIsPressB = False
                  }


buttonEatInput :: GUIData -> GUIState -> ButtonWidget a -> WidgetInput -> 
                  ButtonWidget a
buttonEatInput gd gs button wi =
    let child' = widgetEatInput gd gs (buttonChild button) wi
        button' = helper button wi
    in  button' { buttonChild = child' }
    
    where
      helper button wi =
          let pos = wiPos wi
              pos' = wiPos' wi
              ticks = wiTicks wi
          in case wiType wi of
                -- if touchin without movement for valuePressBTicks, then PressB
              WIDrag    -> let button' = 
                                  if ticks == 0.0 
                                     then button { buttonOK = True }
                                     else button
                               button'' = 
                                  if posIsAlmostEqual pos pos'
                                     then button'
                                     else button' 
                                          {
                                              buttonOK = False,
                                              buttonNotOKTick = guistateTick gs
                                          }

                           in if buttonOK button'' && valuePressBTicks <= ticks
                              then button''
                                   {
                                      buttonIsPressB = True,
                                      buttonOK = False,
                                      buttonNotOKTick = guistateTick gs
                                   }
                              else button''

                -- if released and no movement during touching and touching-time 
                -- strictly less than valuePressBTicks, then PressA
              WIDrop    -> if ticks < valuePressBTicks && buttonOK button
                           then button
                                { 
                                    buttonIsPressA = True,
                                    buttonOK = False,
                                    buttonNotOKTick = guistateTick gs
                                }
                           else button


buttonIterate :: GUIData -> GUIState -> ButtonWidget a -> a -> 
                 IO (ButtonWidget a, a)
buttonIterate gd gs button a = do

    -- modify if button press
    let a' | buttonIsPressA button = buttonActionPressA button a
           | buttonIsPressB button = buttonActionPressB button a
           | otherwise             = a

    -- iterate child
    (child', a'') <- widgetIterate gd gs (buttonChild button) a'
    
    return (button { buttonChild = child' }, a'')



--------------------------------------------------------------------------------
--  additional structure

onPressA :: ButtonWidget a -> (a -> a) -> ButtonWidget a
onPressA button action =
    button { buttonActionPressA = action }


onPressB :: ButtonWidget a -> (a -> a) -> ButtonWidget a
onPressB button action = 
    button { buttonActionPressB = action }


--------------------------------------------------------------------------------
--  make


makeButtonWidget :: Widget w => GUIData -> w a -> ButtonWidget a
makeButtonWidget gd w =
    ButtonWidget
    {
        buttonChild = makeChildWidget gd w,
        buttonActionPressA = id,
        buttonActionPressB = id,
        buttonIsPressA = False,
        buttonIsPressB = False,
        buttonOK = False,
        buttonNotOKTick = 0.0
    }


                  
--------------------------------------------------------------------------------
--  values

valueTweakOK :: Float 
valueTweakOK = 
    0.5

valueTweakTicksInv :: Float
valueTweakTicksInv =
    3.0
        
