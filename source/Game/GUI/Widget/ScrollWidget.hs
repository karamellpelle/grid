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
module Game.GUI.Widget.ScrollWidget
  (
    ScrollWidget,

    makeScrollWidget,
    makeScrollWidgetWth,
    makeScrollWidgetHth,

  ) where

import MyPrelude
import Game.GUI.Widget
import Game.GUI.Widget.Output
import Game.GUI.Widget.Helpers
import Game.GUI.Widget.ChildWidget


-- | scrolls a child widget inside shape
data ScrollWidget a =
    ScrollWidget
    {
        scrollShape :: !GUIShape,
        scrollChild :: !(ChildWidget a),

        scrollIsDrag :: !Bool,
        scrollPos :: !GUIPos,
        scrollPosIdeal :: !GUIPos,
        scrollPosTouched :: !GUIPos,
        scrollTick :: !GUITick,
        scrollTickIdeal :: !GUITick

    }


instance Widget ScrollWidget where
    widgetShape = scrollShape'
    widgetBegin = scrollBegin
    widgetEatInput = scrollEatInput
    widgetIterate = scrollIterate




scrollShape' :: GUIData -> GUIState -> ScrollWidget a -> GUIShape
scrollShape' gd gs scroll =
    scrollShape scroll


scrollBegin :: GUIData -> GUIState -> ScrollWidget a -> IO (ScrollWidget a)
scrollBegin gd gs scroll = do
    child' <- widgetBegin gd gs $ scrollChild scroll
    return $ scroll { scrollChild = child' }



scrollEatInput :: GUIData -> GUIState -> ScrollWidget a -> WidgetInput -> ScrollWidget a
scrollEatInput gd gs scroll wi =
    let scroll' = scrollEat scroll
        child' = childEat scroll' (scrollChild scroll)

    in  scroll' { scrollChild = child' }

    where
      scrollEat scroll =
          case wiType wi of
              WIDrag    -> 
                  if wiIsTouched wi 
                      -- if first touch, set reference pos (fixme: wiPos?)
                      then setIdeal gd gs (wiPos wi) (wiPos' wi) $ 
                           scroll
                           {
                              scrollIsDrag = True,
                              scrollPosTouched = scrollPosIdeal scroll
                           }
                      else setIdeal gd gs (wiPos wi) (wiPos' wi) $ 
                           scroll

              _         ->
                  scroll { scrollIsDrag = False }

      childEat scroll child =
          case gdgsPlusPos gd gs (scrollPos scroll) of
              gs' -> widgetEatInput gd gs' child (wiRefPos wi $ scrollPos scroll)

      setIdeal gd gs (GUIPos x y) (GUIPos x' y') scroll =
          if scrollIsDrag scroll
            then let ax = guistateScaleX gs
                     ay = guistateScaleY gs
                     GUIShape wth hth = scrollShape scroll
                     GUIShape wth' hth' = widgetShape gd gs $ scrollChild scroll
                     GUIPos refx refy = scrollPosTouched scroll
                     idealx = keepInside (ax * (wth - wth')) 0 $ refx + x' - x
                     idealy = keepInside (ay * (hth - hth')) 0 $ refy + y' - y
                 in  scroll { scrollPosIdeal = GUIPos idealx idealy }
            else scroll



scrollIterate :: GUIData -> GUIState -> ScrollWidget a -> a ->
                 IO (ScrollWidget a, a)
scrollIterate gd gs scroll a = do
 
    -- output --
    let shape = scrollShape scroll

    -- set cut 
    beginCut gd gs
    useTexStencil gd gs 0 0 0
    gs' <- useNoFillTex gd gs
    draw4Shape gd gs' shape
  

    -- iterate child
    insideCut gd gs'
    gs'' <- plusPos' gd gs' $ scrollPos scroll 
    (child', a') <- widgetIterate gd gs'' (scrollChild scroll) a
    endCut gd gs''
   

    -- reset
    resetPos gd gs
    resetFillTex gd gs

    -- step --
    let tick = scrollTick scroll
        tick' = guistateTick gs
        pos' = stepDT (rTF tick' - rTF tick) (scrollPos scroll) (scrollPosIdeal scroll)

        scroll' = scroll 
                  {
                      scrollChild = child', 
                      scrollPos = pos',
                      scrollTick = tick'
                  }

    return (scroll', a')
    
    where
      stepDT dt (GUIPos x y) (GUIPos x' y') =
          GUIPos (helper dt x x') (helper dt y y')
          where
            helper dt z z' = 
                if z <= z'
                    then min (z + dt * speed) z'
                    else max (z - dt * speed) z'
            speed = 1



--------------------------------------------------------------------------------
--  

makeScrollWidget :: Widget w => GUIData -> GUIShape -> w a -> ScrollWidget a
makeScrollWidget gd shape w =
    ScrollWidget
    {
        scrollShape = shape,
        scrollChild = makeChildWidget gd w,
        
        scrollIsDrag = False,
        scrollPos = GUIPos 0.0 0.0,
        scrollPosIdeal = GUIPos 0.0 0.0,
        scrollPosTouched = GUIPos 0.0 0.0,
        scrollTick = 0.0,
        scrollTickIdeal = 0.0
    }


-- make ScrollWidget using (current) hth of child
makeScrollWidgetWth :: Widget w => GUIData -> Float -> w a -> ScrollWidget a
makeScrollWidgetWth gd wth w =
    case widgetShape gd (error "makeScrollWidget: child depends on GUIState") w of
        GUIShape wth' hth'  -> 
            ScrollWidget
            {
                scrollShape = GUIShape wth hth',
                scrollChild = makeChildWidget gd w,
                
                scrollIsDrag = False,
                scrollPos = GUIPos 0.0 0.0,
                scrollPosIdeal = GUIPos 0.0 0.0,
                scrollPosTouched = GUIPos 0.0 0.0,
                scrollTick = 0.0,
                scrollTickIdeal = 0.0
            }



-- make ScrollWidget using (current) wth of child
makeScrollWidgetHth :: Widget w => GUIData -> Float -> w a -> ScrollWidget a
makeScrollWidgetHth gd hth w =
    case widgetShape gd (error "makeScrollWidget: child depends on GUIState") w of
        GUIShape wth' hth'  -> 
            ScrollWidget
            {
                scrollShape = GUIShape wth' hth,
                scrollChild = makeChildWidget gd w,
                
                scrollIsDrag = False,
                scrollPos = GUIPos 0.0 0.0,
                scrollPosIdeal = GUIPos 0.0 0.0,
                scrollPosTouched = GUIPos 0.0 0.0,
                scrollTick = 0.0,
                scrollTickIdeal = 0.0
            }





