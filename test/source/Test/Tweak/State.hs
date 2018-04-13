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
module Test.Tweak.State
  (
    TestTweakState (..),
    TweakWidget,
    TweakA (..),
   
    makeTestTweakState,
    makeTweakWidget,
    makeTweakA,
    aSetConstI,
    aSetConstJ,
    aSetConstK,
    aSetConstL,
    constToSlide,

  ) where


import MyPrelude
import Game

import Game.Run.RunWorld
import Game.GUI
import Game.GUI.Widget.BorderWidget
import Game.GUI.Widget.SlideWidget
import Game.GUI.Widget.LayoutWidget
import Game.GUI.Widget.ScreenWidget


data TestTweakState =
    TestTweakState
    {
        ttstateWidget :: !TweakWidget,
        ttstateA :: !TweakA,
        ttstateColorIx :: !UInt
    }


makeTestTweakState :: RunWorld -> MEnv' TestTweakState
makeTestTweakState run = do
    gd <- resourceGUIData
    let tweak = sceneTweak $ runScene run
        w = makeTweakWidget gd tweak
        a = makeTweakA tweak
    return TestTweakState
           {
              ttstateWidget = w,
              ttstateA = a,
              ttstateColorIx = 0
           }


type TweakWidget =
    ScreenWidget TweakA

data TweakA =
    TweakA
    {
        aTouch :: !Bool,
        aConstI :: !Float,
        aConstJ :: !Float,
        aConstK :: !Float,
        aConstL :: !Float
    }

makeTweakA :: Tweak -> TweakA
makeTweakA tweak =
    TweakA
    {   
        aTouch = False,
        aConstI = tweakConstI tweak,
        aConstJ = tweakConstJ tweak,
        aConstK = tweakConstK tweak,
        aConstL = tweakConstL tweak
    }


makeTweakWidget :: GUIData -> Tweak -> TweakWidget
makeTweakWidget gd tweak = 
    makeScreenWidget gd 0.5 0.88 $ 
        makeBorderWidget gd $ 
        makeLayoutWidgetStatic gd $ ygather $ map (makeChildLayout . widget) [
#ifdef GRID_TEST_COLORFY
                       (makeSlideWidget gd 0.9 0.0 (constToSlide $ tweakConstI tweak)
                       `onNewValue` (\value a -> aSetConstI a value)
                       ),
                       (makeSlideWidget gd 0.9 0.0 (constToSlide $ tweakConstJ tweak)
                        `onNewValue` (\value a -> aSetConstJ a value)
                       ),
                       (makeSlideWidget gd 0.9 0.0 (constToSlide $ tweakConstK tweak)
                        `onNewValue` (\value a -> aSetConstK a value)
                       ),
                       (makeSlideWidget gd 0.9 0.0 (constToSlide $ tweakConstL tweak)
                        `onNewValue` (\value a -> aSetConstL a value)
                       )
#endif
                       ] 
--------------------------------------------------------------------------------
--  

aSetConstI :: TweakA -> Float -> TweakA
aSetConstI a value = 
    a { aConstI = fromSlide constMin constMax value, aTouch = True }

aSetConstJ :: TweakA -> Float -> TweakA
aSetConstJ a value = 
    a { aConstJ = fromSlide constMin constMax value, aTouch = True }

aSetConstK :: TweakA -> Float -> TweakA
aSetConstK a value = 
    a { aConstK = fromSlide constMin constMax value, aTouch = True }

aSetConstL :: TweakA -> Float -> TweakA
aSetConstL a value = 
    a { aConstL = fromSlide constMin constMax value, aTouch = True }

fromSlide :: Float -> Float -> Float -> Float 
fromSlide min max =
  smooth min max


constToSlide :: Float -> Float
constToSlide =
    toSlide constMin constMax

toSlide :: Float -> Float -> Float -> Float
toSlide min max v =
    (v - min) / (max - min)

#ifdef GRID_TEST_PULSE0
constMin :: Float
constMin = -20.0

constMax :: Float
constMax = 20.0

#else

constMin :: Float
constMin = -6.0

constMax :: Float
constMax = 6.0

#endif


