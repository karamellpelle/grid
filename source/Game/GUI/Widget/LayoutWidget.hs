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
module Game.GUI.Widget.LayoutWidget
  (
    LayoutWidget,

    makeLayoutWidget,
    makeLayoutWidgetStatic,
 
    module Game.GUI.Widget.ChildWidget,
    module Game.GUI.Widget.LayoutWidget.StaticLayout,

  ) where



import MyPrelude
import Game.GUI.Widget
import Game.GUI.Widget.Output
import Game.GUI.Widget.Helpers
import Game.GUI.Widget.ChildWidget
import Game.GUI.Widget.LayoutWidget.StaticLayout


data LayoutWidget a =
    LayoutWidget
    {
        layoutElements :: ![LayoutElement a],
        layoutShape :: !GUIShape

    }


data LayoutElement a =
    LayoutElement
    {
        lePos :: !GUIPos,
        leChild :: !(ChildWidget a)
    }


instance Widget LayoutWidget where
    widgetShape = \gd gs w -> layoutShape w
    widgetBegin = layoutBegin
    widgetEatInput = layoutEatInput
    widgetIterate = layoutIterate



--------------------------------------------------------------------------------
--  Widget structure



layoutBegin :: GUIData -> GUIState -> LayoutWidget a -> IO (LayoutWidget a)
layoutBegin gd gs layout = do
    let es = layoutElements layout
    es' <- mapESBegin es
    return $ layout { layoutElements = es' }

    where
      mapESBegin es = case es of
          []      -> return []
          (e:es)  -> do
              child' <- widgetBegin gd gs (leChild e)
              es' <- mapESBegin es
              return (e { leChild = child' } : es')


layoutEatInput :: GUIData -> GUIState -> LayoutWidget a -> WidgetInput -> LayoutWidget a
layoutEatInput gd gs layout wi =
    let es' = mapESEatInput (layoutElements layout)
    in  layout { layoutElements = es' }
    
    where
      mapESEatInput es = case es of
          []      -> 
              []
          (e:es)  -> 
              let child = leChild e
                  pos = lePos e
                  shape = widgetShape gd gs child
              in  case gdgsIsInputInside gd gs pos shape wi of
                  Nothing   -> 
                      (e:mapESEatInput es)
                  Just gs'  -> 
                      let child' = widgetEatInput gd gs' child wi
                      in  (e { leChild = child' } : mapESEatInput es)
                      --in  (e { leChild = child' } : es)


layoutIterate :: GUIData -> GUIState -> LayoutWidget a -> a -> IO (LayoutWidget a, a)
layoutIterate gd gs layout a = do

    -- iterate children
    let es = layoutElements layout
    (es', a') <- mapESIterate gd gs es a
    
    return (layout { layoutElements = es' }, a')
    
    where
      mapESIterate gd gs es a = case es of
          []      -> return ([], a)
          (e:es)  -> do
               
              -- iterate child
              gs' <- plusPos gd gs (lePos e)
              (child', a') <- widgetIterate gd gs' (leChild e) a
              resetPos gd gs
            
              -- iterate children
              (es', a'') <- mapESIterate gd gs es a'
              return (e { leChild = child' } : es', a'')
    



--------------------------------------------------------------------------------
--  make 


makeLayoutWidget :: GUIData -> GUIShape -> [(GUIPos, ChildWidget a)] -> LayoutWidget a
makeLayoutWidget gd shape = \pws ->
    LayoutWidget 
    {
        layoutShape = shape,
        layoutElements = map (\(pos, w) -> LayoutElement pos w) pws
    }


makeLayoutWidgetStatic :: StaticLayout layout => GUIData -> layout a -> LayoutWidget a
makeLayoutWidgetStatic gd = \slayout ->
    makeLayoutWidget gd (slayoutShape gd slayout) (slayoutChildren gd slayout)

