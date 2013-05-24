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
--------------------------------------------------------------------------------
--  this module is ugly, not functionally written but object oriented, 
--  and should be removed or fixed!!!

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Game.GUI.Widget.LayoutWidget.StaticLayout
  (
    StaticLayout (..),
    makeChildLayout,

    xgather,
    xgather0,
    xgather1,
    xgather2,
    xgather3,
    xgatherTop,
    xgatherTop0,
    xgatherTop1,
    xgatherTop2,
    xgatherTop3,
    xgatherBot,
    xgatherBot0,
    xgatherBot1,
    xgatherBot2,
    xgatherBot3,
    xgatherFill,
    xgatherFill0,
    xgatherFill1,
    xgatherFill2,
    xgatherFill3,
    xgatherTopFill,
    xgatherTopFill0,
    xgatherTopFill1,
    xgatherTopFill2,
    xgatherTopFill3,
    xgatherBotFill,
    xgatherBotFill0,
    xgatherBotFill1,
    xgatherBotFill2,
    xgatherBotFill3,

    ygather,
    ygather0,
    ygather1,
    ygather2,
    ygather3,
    ygatherLeft,
    ygatherLeft0,
    ygatherLeft1,
    ygatherLeft2,
    ygatherLeft3,
    ygatherLeft4,
    ygatherRight,
    ygatherRight0,
    ygatherRight1,
    ygatherRight2,
    ygatherRight3,
    ygatherFill,
    ygatherFill0,
    ygatherFill1,
    ygatherFill2,
    ygatherFill3,
    ygatherLeftFill,
    ygatherLeftFill0,
    ygatherLeftFill1,
    ygatherLeftFill2,
    ygatherLeftFill3,
    ygatherLeftFill4,
    ygatherRightFill,
    ygatherRightFill0,
    ygatherRightFill1,
    ygatherRightFill2,
    ygatherRightFill3,

    xfill,
    xfillLeft,
    xfillRight,
    yfill,
    yfillTop,
    yfillBot,

    xapart,
    yapart,

    WidgetLayout,
    widget,

) where

import MyPrelude
import Game.GUI.GUIData
import Game.GUI.GUIState
import Game.GUI.Widget
import Game.GUI.Widget.Helpers
import Game.GUI.Widget.ChildWidget


--------------------------------------------------------------------------------
--  class StaticLayout

class StaticLayout l where
    slayoutShape :: GUIData -> l a -> GUIShape
    slayoutChildren :: GUIData -> l a -> [(GUIPos, ChildWidget a)]



--------------------------------------------------------------------------------
-- functions
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
--  xgather

xgather :: [ChildLayout a] -> XGatherLayout a
xgather childs = 
    makeXGatherLayout 0.0 0.5 childs

xgather0 :: XGatherLayout a
xgather0 = 
    xgather []

xgather1 :: (StaticLayout l0) => 
            l0 a -> XGatherLayout a
xgather1 l0 = 
    xgather [makeChildLayout l0]


xgather2 :: (StaticLayout l0, StaticLayout l1) => 
            l0 a -> l1 a -> XGatherLayout a
xgather2 l0 l1 = 
    xgather [makeChildLayout l0, makeChildLayout l1]


xgather3 :: (StaticLayout l0, StaticLayout l1, StaticLayout l2) => 
            l0 a -> l1 a -> l2 a -> XGatherLayout a
xgather3 l0 l1 l2 = 
    xgather [makeChildLayout l0, makeChildLayout l1, makeChildLayout l2]



xgatherTop :: [ChildLayout a] -> XGatherLayout a
xgatherTop childs = 
    makeXGatherLayout 0.0 0.0 childs


xgatherTop0 :: XGatherLayout a
xgatherTop0 = 
    xgatherTop []


xgatherTop1 :: (StaticLayout l0) => 
               l0 a -> XGatherLayout a
xgatherTop1 l0 = 
    xgatherTop [makeChildLayout l0]


xgatherTop2 :: (StaticLayout l0, StaticLayout l1) => 
               l0 a -> l1 a -> XGatherLayout a
xgatherTop2 l0 l1 = 
    xgatherTop [makeChildLayout l0, makeChildLayout l1]


xgatherTop3 :: (StaticLayout l0, StaticLayout l1, StaticLayout l2) => 
               l0 a -> l1 a -> l2 a -> XGatherLayout a
xgatherTop3 l0 l1 l2 = 
    xgatherTop [makeChildLayout l0, makeChildLayout l1, makeChildLayout l2]

xgatherBot :: [ChildLayout a] -> XGatherLayout a
xgatherBot childs = 
    makeXGatherLayout 0.0 1.0 childs


xgatherBot0 :: XGatherLayout a
xgatherBot0 = 
    xgatherBot []


xgatherBot1 :: (StaticLayout l0) => 
               l0 a -> XGatherLayout a
xgatherBot1 l0 = 
    xgatherBot [makeChildLayout l0]


xgatherBot2 :: (StaticLayout l0, StaticLayout l1) => 
               l0 a -> l1 a -> XGatherLayout a
xgatherBot2 l0 l1 = 
    xgatherBot [makeChildLayout l0, makeChildLayout l1]


xgatherBot3 :: (StaticLayout l0, StaticLayout l1, StaticLayout l2) => 
               l0 a -> l1 a -> l2 a -> XGatherLayout a
xgatherBot3 l0 l1 l2 = 
    xgatherBot [makeChildLayout l0, makeChildLayout l1, makeChildLayout l2]



--------------------------------------------------------------------------------
--  xgatherFill

xgatherFill :: Float -> [ChildLayout a] -> XGatherLayout a
xgatherFill hth childs = 
    makeXGatherLayout hth 0.5 childs

xgatherFill0 :: Float -> XGatherLayout a
xgatherFill0 hth= 
    xgatherFill hth []

xgatherFill1 :: (StaticLayout l0) => 
                Float -> l0 a -> XGatherLayout a
xgatherFill1 hth l0 = 
    xgatherFill hth [makeChildLayout l0]


xgatherFill2 :: (StaticLayout l0, StaticLayout l1) => 
                Float -> l0 a -> l1 a -> XGatherLayout a
xgatherFill2 hth l0 l1 = 
    xgatherFill hth [makeChildLayout l0, makeChildLayout l1]


xgatherFill3 :: (StaticLayout l0, StaticLayout l1, StaticLayout l2) => 
                Float -> l0 a -> l1 a -> l2 a -> XGatherLayout a
xgatherFill3 hth l0 l1 l2 = 
    xgatherFill hth [makeChildLayout l0, makeChildLayout l1, makeChildLayout l2]



xgatherTopFill :: Float -> [ChildLayout a] -> XGatherLayout a
xgatherTopFill hth childs = 
    makeXGatherLayout hth 0.0 childs


xgatherTopFill0 :: Float -> XGatherLayout a
xgatherTopFill0 hth = 
    xgatherTopFill hth []


xgatherTopFill1 :: (StaticLayout l0) => 
                   Float -> l0 a -> XGatherLayout a
xgatherTopFill1 hth l0 = 
    xgatherTopFill hth [makeChildLayout l0]


xgatherTopFill2 :: (StaticLayout l0, StaticLayout l1) => 
                   Float -> l0 a -> l1 a -> XGatherLayout a
xgatherTopFill2 hth l0 l1 = 
    xgatherTopFill hth [makeChildLayout l0, makeChildLayout l1]


xgatherTopFill3 :: (StaticLayout l0, StaticLayout l1, StaticLayout l2) => 
                   Float -> l0 a -> l1 a -> l2 a -> XGatherLayout a
xgatherTopFill3 hth l0 l1 l2 = 
    xgatherTopFill hth [makeChildLayout l0, makeChildLayout l1, makeChildLayout l2]

xgatherBotFill :: Float -> [ChildLayout a] -> XGatherLayout a
xgatherBotFill hth childs = 
    makeXGatherLayout hth 1.0 childs


xgatherBotFill0 :: Float -> XGatherLayout a
xgatherBotFill0 hth = 
    xgatherBotFill hth []


xgatherBotFill1 :: (StaticLayout l0) => 
                   Float -> l0 a -> XGatherLayout a
xgatherBotFill1 hth l0 = 
    xgatherBotFill hth [makeChildLayout l0]


xgatherBotFill2 :: (StaticLayout l0, StaticLayout l1) => 
                   Float -> l0 a -> l1 a -> XGatherLayout a
xgatherBotFill2 hth l0 l1 = 
    xgatherBotFill hth [makeChildLayout l0, makeChildLayout l1]


xgatherBotFill3 :: (StaticLayout l0, StaticLayout l1, StaticLayout l2) => 
                   Float -> l0 a -> l1 a -> l2 a -> XGatherLayout a
xgatherBotFill3 hth l0 l1 l2 = 
    xgatherBotFill hth [makeChildLayout l0, makeChildLayout l1, makeChildLayout l2]



--------------------------------------------------------------------------------
--  ygather

ygather :: [ChildLayout a] -> YGatherLayout a
ygather childs = 
    makeYGatherLayout 0.0 0.5 childs

ygather0 :: YGatherLayout a
ygather0 = 
    ygather []

ygather1 :: (StaticLayout l0) => 
            l0 a -> YGatherLayout a
ygather1 l0 = 
    ygather [makeChildLayout l0]


ygather2 :: (StaticLayout l0, StaticLayout l1) => 
            l0 a -> l1 a -> YGatherLayout a
ygather2 l0 l1 = 
    ygather [makeChildLayout l0, makeChildLayout l1]


ygather3 :: (StaticLayout l0, StaticLayout l1, StaticLayout l2) => 
            l0 a -> l1 a -> l2 a -> YGatherLayout a
ygather3 l0 l1 l2 = 
    ygather [makeChildLayout l0, makeChildLayout l1, makeChildLayout l2]



ygatherLeft :: [ChildLayout a] -> YGatherLayout a
ygatherLeft childs = 
    makeYGatherLayout 0.0 0.0 childs


ygatherLeft0 :: YGatherLayout a
ygatherLeft0 = 
    ygatherLeft []


ygatherLeft1 :: (StaticLayout l0) => 
               l0 a -> YGatherLayout a
ygatherLeft1 l0 = 
    ygatherLeft [makeChildLayout l0]


ygatherLeft2 :: (StaticLayout l0, StaticLayout l1) => 
               l0 a -> l1 a -> YGatherLayout a
ygatherLeft2 l0 l1 = 
    ygatherLeft [makeChildLayout l0, makeChildLayout l1]


ygatherLeft3 :: (StaticLayout l0, StaticLayout l1, StaticLayout l2) => 
               l0 a -> l1 a -> l2 a -> YGatherLayout a
ygatherLeft3 l0 l1 l2 = 
    ygatherLeft [makeChildLayout l0, makeChildLayout l1, makeChildLayout l2]

ygatherLeft4 :: (StaticLayout l0, StaticLayout l1, StaticLayout l2, 
                StaticLayout l3) => 
                l0 a -> l1 a -> l2 a -> l3 a -> YGatherLayout a
ygatherLeft4 l0 l1 l2 l3 = 
    ygatherLeft [makeChildLayout l0, makeChildLayout l1, 
                 makeChildLayout l2, makeChildLayout l3]


ygatherRight :: [ChildLayout a] -> YGatherLayout a
ygatherRight childs = 
    makeYGatherLayout 0.0 1.0 childs


ygatherRight0 :: YGatherLayout a
ygatherRight0 = 
    ygatherRight []


ygatherRight1 :: (StaticLayout l0) => 
               l0 a -> YGatherLayout a
ygatherRight1 l0 = 
    ygatherRight [makeChildLayout l0]


ygatherRight2 :: (StaticLayout l0, StaticLayout l1) => 
               l0 a -> l1 a -> YGatherLayout a
ygatherRight2 l0 l1 = 
    ygatherRight [makeChildLayout l0, makeChildLayout l1]


ygatherRight3 :: (StaticLayout l0, StaticLayout l1, StaticLayout l2) => 
               l0 a -> l1 a -> l2 a -> YGatherLayout a
ygatherRight3 l0 l1 l2 = 
    ygatherRight [makeChildLayout l0, makeChildLayout l1, makeChildLayout l2]



--------------------------------------------------------------------------------
--  ygatherFill

ygatherFill :: Float -> [ChildLayout a] -> YGatherLayout a
ygatherFill hth childs = 
    makeYGatherLayout hth 0.5 childs

ygatherFill0 :: Float -> YGatherLayout a
ygatherFill0 hth= 
    ygatherFill hth []

ygatherFill1 :: (StaticLayout l0) => 
                Float -> l0 a -> YGatherLayout a
ygatherFill1 hth l0 = 
    ygatherFill hth [makeChildLayout l0]


ygatherFill2 :: (StaticLayout l0, StaticLayout l1) => 
                Float -> l0 a -> l1 a -> YGatherLayout a
ygatherFill2 hth l0 l1 = 
    ygatherFill hth [makeChildLayout l0, makeChildLayout l1]


ygatherFill3 :: (StaticLayout l0, StaticLayout l1, StaticLayout l2) => 
                Float -> l0 a -> l1 a -> l2 a -> YGatherLayout a
ygatherFill3 hth l0 l1 l2 = 
    ygatherFill hth [makeChildLayout l0, makeChildLayout l1, makeChildLayout l2]



ygatherLeftFill :: Float -> [ChildLayout a] -> YGatherLayout a
ygatherLeftFill hth childs = 
    makeYGatherLayout hth 0.0 childs


ygatherLeftFill0 :: Float -> YGatherLayout a
ygatherLeftFill0 hth = 
    ygatherLeftFill hth []


ygatherLeftFill1 :: (StaticLayout l0) => 
                   Float -> l0 a -> YGatherLayout a
ygatherLeftFill1 hth l0 = 
    ygatherLeftFill hth [makeChildLayout l0]


ygatherLeftFill2 :: (StaticLayout l0, StaticLayout l1) => 
                   Float -> l0 a -> l1 a -> YGatherLayout a
ygatherLeftFill2 hth l0 l1 = 
    ygatherLeftFill hth [makeChildLayout l0, makeChildLayout l1]


ygatherLeftFill3 :: (StaticLayout l0, StaticLayout l1, StaticLayout l2) => 
                   Float -> l0 a -> l1 a -> l2 a -> YGatherLayout a
ygatherLeftFill3 hth l0 l1 l2 = 
    ygatherLeftFill hth [makeChildLayout l0, makeChildLayout l1, makeChildLayout l2]

ygatherLeftFill4 :: 
    (StaticLayout l0, StaticLayout l1, StaticLayout l2, StaticLayout l3) => 
    Float -> l0 a -> l1 a -> l2 a -> l3 a -> YGatherLayout a
ygatherLeftFill4 hth l0 l1 l2 l3 = 
    ygatherLeftFill hth [makeChildLayout l0,
                         makeChildLayout l1, 
                         makeChildLayout l2,
                         makeChildLayout l3]


ygatherRightFill :: Float -> [ChildLayout a] -> YGatherLayout a
ygatherRightFill hth childs = 
    makeYGatherLayout hth 1.0 childs


ygatherRightFill0 :: Float -> YGatherLayout a
ygatherRightFill0 hth = 
    ygatherRightFill hth []


ygatherRightFill1 :: (StaticLayout l0) => 
                   Float -> l0 a -> YGatherLayout a
ygatherRightFill1 hth l0 = 
    ygatherRightFill hth [makeChildLayout l0]


ygatherRightFill2 :: (StaticLayout l0, StaticLayout l1) => 
                   Float -> l0 a -> l1 a -> YGatherLayout a
ygatherRightFill2 hth l0 l1 = 
    ygatherRightFill hth [makeChildLayout l0, makeChildLayout l1]


ygatherRightFill3 :: (StaticLayout l0, StaticLayout l1, StaticLayout l2) => 
                   Float -> l0 a -> l1 a -> l2 a -> YGatherLayout a
ygatherRightFill3 hth l0 l1 l2 = 
    ygatherRightFill hth [makeChildLayout l0, makeChildLayout l1, makeChildLayout l2]




--------------------------------------------------------------------------------
--  fill

xfill :: StaticLayout l =>
         Float -> l a -> YGatherLayout a
xfill wth layout =
    makeYGatherLayout wth 0.5 [makeChildLayout layout]


xfillLeft :: StaticLayout l =>
             Float -> l a -> YGatherLayout a
xfillLeft wth layout =
    makeYGatherLayout wth 0.0 [makeChildLayout layout]


xfillRight :: StaticLayout l =>
              Float -> l a -> YGatherLayout a
xfillRight wth layout =
    makeYGatherLayout wth 1.0 [makeChildLayout layout]


yfill :: StaticLayout l =>
         Float -> l a -> XGatherLayout a
yfill hth layout =
    makeXGatherLayout hth 0.5 [makeChildLayout layout]


yfillTop :: StaticLayout l =>
            Float -> l a -> XGatherLayout a
yfillTop hth layout =
    makeXGatherLayout hth 0.0 [makeChildLayout layout]


yfillBot :: StaticLayout l =>
            Float -> l a -> XGatherLayout a
yfillBot hth layout =
    makeXGatherLayout hth 1.0 [makeChildLayout layout]



--------------------------------------------------------------------------------
--  apart

xapart :: (StaticLayout l0, StaticLayout l1) =>
          Float -> l0 a -> l1 a -> XGatherLayout a
xapart wth l0 l1 = 
    let wth' = 0.5 * wth
    in  xgather2 (xfillLeft wth' l0) (xfillRight wth' l1)


yapart :: (StaticLayout l0, StaticLayout l1) =>
          Float -> l0 a -> l1 a -> YGatherLayout a
yapart hth l0 l1 = 
    let hth' = 0.5 * hth
    in  ygather2 (yfillTop hth' l0) (yfillBot hth' l1)





--------------------------------------------------------------------------------
--  ChildLayout

data ChildLayout a =
    ChildLayout
    {
        childShape :: GUIData -> GUIShape,
        childChildren :: GUIData -> [ (GUIPos, ChildWidget a) ]
    }

instance StaticLayout ChildLayout where
    slayoutShape gd child  =
        childShape child gd

    slayoutChildren gd child =
        childChildren child gd


makeChildLayout :: StaticLayout l => l a -> ChildLayout a
makeChildLayout layout =
    ChildLayout
    {
        childShape = \gd -> slayoutShape gd layout,
        childChildren = \gd -> slayoutChildren gd layout
    }


--------------------------------------------------------------------------------
--  XGatherLayout


data XGatherLayout a =
    XGatherLayout
    {
        xgatherMinHth :: Float,
        xgatherAlign :: Float,
        xgatherChildLayouts :: [ChildLayout a]
    }


instance StaticLayout XGatherLayout where
    slayoutShape = xgatherShape
    slayoutChildren = xgatherChildren


xgatherShape :: GUIData -> XGatherLayout a -> GUIShape
xgatherShape gd xgather = 
    foldl helper (GUIShape 0.0 (xgatherMinHth xgather)) $ xgatherChildLayouts xgather
    where
      helper (GUIShape wth hth) child =
          case slayoutShape gd child of
              GUIShape wth' hth'  -> GUIShape (wth + wth') (max hth hth')


xgatherChildren :: GUIData -> XGatherLayout a -> [(GUIPos, ChildWidget a)]
xgatherChildren gd xgather = 
    let hth = maxHth (xgatherMinHth xgather) $ xgatherChildLayouts xgather
        ay = xgatherAlign xgather
    in  helper ay hth 0.0 $ xgatherChildLayouts xgather
    where
      maxHth hth [] =
          hth
      maxHth hth (l:ls) =
          case slayoutShape gd l of
              GUIShape wth' hth' -> maxHth (max hth hth') ls

      helper ay hth x (child:childs) =
          let GUIShape wth' hth' = slayoutShape gd child
              pos' = GUIPos x (ay * (hth - hth'))
          in  map (\(pos'', w) -> (pos' `posPlus` pos'', w)) (slayoutChildren gd child) ++
              helper ay hth (x + wth') childs
      helper ay hth pos [] =
          []


makeXGatherLayout :: Float -> Float -> [ChildLayout a] -> XGatherLayout a
makeXGatherLayout hth align childs =
    XGatherLayout
    {
        xgatherMinHth = hth,
        xgatherAlign = align,
        xgatherChildLayouts = childs
    }


--------------------------------------------------------------------------------
--  YGatherLayout


data YGatherLayout a =
    YGatherLayout
    {
        ygatherMinWth :: Float,
        ygatherAlign :: Float,
        ygatherChildLayouts :: [ChildLayout a]
    }


instance StaticLayout YGatherLayout where
    slayoutShape = ygatherShape
    slayoutChildren = ygatherChildren


ygatherShape :: GUIData -> YGatherLayout a -> GUIShape
ygatherShape gd ygather = 
    foldl helper (GUIShape (ygatherMinWth ygather) 0.0) $ ygatherChildLayouts ygather
    where
      helper (GUIShape wth hth) child =
          case slayoutShape gd child of
              GUIShape wth' hth'  -> GUIShape (max wth wth') (hth + hth')


ygatherChildren :: GUIData -> YGatherLayout a -> [(GUIPos, ChildWidget a)]
ygatherChildren gd ygather = 
    let wth = maxWth (ygatherMinWth ygather) $ ygatherChildLayouts ygather
        ax = ygatherAlign ygather
    in  helper ax wth 0.0 $ ygatherChildLayouts ygather
    where
      maxWth wth [] =
          wth
      maxWth wth (l:ls) =
          case slayoutShape gd l of
              GUIShape wth' hth' -> maxWth (max wth wth') ls

      helper ax wth y (child:childs) =
          let GUIShape wth' hth' = slayoutShape gd child
              pos' = GUIPos (ax * (wth - wth')) y
          in  map (\(pos'', w) -> (pos' `posPlus` pos'', w)) (slayoutChildren gd child) ++
              helper ax wth (y + hth') childs
      helper ax hth pos [] =
          []


makeYGatherLayout :: Float -> Float -> [ChildLayout a] -> YGatherLayout a
makeYGatherLayout wth align childs =
    YGatherLayout
    {
        ygatherMinWth = wth,
        ygatherAlign = align,
        ygatherChildLayouts = childs
    }


--------------------------------------------------------------------------------
--  Widget layouts

data WidgetLayout a =
    WidgetLayout
    {
        wlayoutChild :: !(ChildWidget a)
    }


widget :: Widget w => w a -> WidgetLayout a
widget w =
    WidgetLayout (makeChildWidget undefGUIData w)


instance StaticLayout WidgetLayout where
    slayoutShape = \gd la -> widgetShape gd undefGUIState (wlayoutChild la)
    slayoutChildren = \gd la -> [(GUIPos 0.0 0.0, wlayoutChild la)]


undefGUIState :: GUIState
undefGUIState = error "StaticLayout: undefined GUIState"

undefGUIData :: GUIData
undefGUIData = error "StaticLayout: undefined GUIData"


{-
instance (Widget w => StaticLayout w) where
    slayoutShape gd w =
        widgetShape gd undefGUIState w

    slayoutChildren gd w =
        [ (GUIPos 0.0 0.0, makeChildWidget gd w) ]
-}

-- the above caused overlapping instances, so lets define instances for all Widgets :(

{-
instance StaticLayout ActiveButtonWidget where
  slayoutShape gd w =
        widgetShape gd (undefGUIState "ActiveButtonWidget") w
  slayoutChildren gd w = 
        [ (GUIPos 0.0 0.0, makeChildWidget gd w) ]

instance StaticLayout ButtonWidget where
  slayoutShape gd w =
        widgetShape gd (undefGUIState "ButtonWidget") w
  slayoutChildren gd w = 
        [ (GUIPos 0.0 0.0, makeChildWidget gd w) ]


instance StaticLayout ChildWidget where
  slayoutShape gd w =
        widgetShape gd (undefGUIState "ChildWidget") w
  slayoutChildren gd w = 
        [ (GUIPos 0.0 0.0, makeChildWidget gd w) ]

instance StaticLayout ContourWidget where
  slayoutShape gd w =
        widgetShape gd (undefGUIState "ContourWidget") w
  slayoutChildren gd w = 
        [ (GUIPos 0.0 0.0, makeChildWidget gd w) ]


instance StaticLayout EmptyWidget where
  slayoutShape gd w =
        widgetShape gd (undefGUIState "EmptyWidget") w
  slayoutChildren gd w = 
        [ (GUIPos 0.0 0.0, makeChildWidget gd w) ]

instance StaticLayout LabelWidget where
  slayoutShape gd w = 
      widgetShape gd (undefGUIState "LabelWidget") w
  slayoutChildren gd w = 
      [ (GUIPos 0.0 0.0, makeChildWidget gd w) ]


instance StaticLayout MinWidget where
  slayoutShape gd w = 
      widgetShape gd (undefGUIState "MinWidget") w
  slayoutChildren gd w = 
      [ (GUIPos 0.0 0.0, makeChildWidget gd w) ]


instance StaticLayout MaxWidget where
  slayoutShape gd w = 
      widgetShape gd (undefGUIState "MaxWidget") w
  slayoutChildren gd w = 
      [ (GUIPos 0.0 0.0, makeChildWidget gd w) ]


instance StaticLayout NumberWidget where
  slayoutShape gd w = 
      widgetShape gd (undefGUIState "NumberWidget") w
  slayoutChildren gd w = 
      [ (GUIPos 0.0 0.0, makeChildWidget gd w) ]


instance StaticLayout ScaleWidget where
  slayoutShape gd w = 
      widgetShape gd (undefGUIState "ScaleWidget") w
  slayoutChildren gd w = 
      [ (GUIPos 0.0 0.0, makeChildWidget gd w) ]


instance StaticLayout ScreenWidget where
  slayoutShape gd w = 
      widgetShape gd (undefGUIState "ScreenWidget") w
  slayoutChildren gd w = 
      [ (GUIPos 0.0 0.0, makeChildWidget gd w) ]

instance StaticLayout ScrollWidget where
  slayoutShape gd w =
      widgetShape gd (undefGUIState "ScrollWidget") w
  slayoutChildren gd w = 
      [ (GUIPos 0.0 0.0, makeChildWidget gd w) ]

instance StaticLayout SlideWidget where
  slayoutShape gd w = 
      widgetShape gd (undefGUIState "SlideWidget") w
  slayoutChildren gd w = 
      [ (GUIPos 0.0 0.0, makeChildWidget gd w) ]

instance StaticLayout TextWidget where
  slayoutShape gd w =
      widgetShape gd (undefGUIState "TextWidget") w
  slayoutChildren gd w = 
      [ (GUIPos 0.0 0.0, makeChildWidget gd w) ]


  

undefGUIState :: String -> GUIState
undefGUIState name =
    error $ name ++ " not allowed in static layout (depends on GUIState)"
-}
