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
module Game.Run.Iteration.About
  (
    iterationDoAbout,

  ) where


import MyPrelude
import Game

import Game.Grid.Helpers
import Game.Run.RunWorld
import Game.Run.Do
import Game.Run.Output
import Game.Run.Helpers
import Game.Run.Helpers.Make
import Game.Run.Iteration.Iteration

import System.Random

import Game.GUI
import Game.GUI.Widget.ScreenWidget
import Game.GUI.Widget.ScrollWidget
import Game.GUI.Widget.TextWidget
import Game.GUI.Widget.ButtonWidget
import Game.GUI.Widget.LabelWidget
import Game.GUI.Widget.ContourWidget
import Game.GUI.Widget.BorderWidget

--------------------------------------------------------------------------------
--  text

text :: [String]
text = [
      " this game is written in Haskell   ",
      " ",
      "            * CREDITS *            ",
      " the haskell community ",
      " the open source community ", 
      " the internets ",
      " www.humus.name ",
      "",
      "abcdefghijklmnopqrstuvwxyz&[{}(=*)+",
      "]!#`%0123456789/?@^-_\\|;:,<.> $~",
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ]


data WidgetA =
    AEmpty      |
    AEscape


type WidgetT =
    ButtonWidget WidgetA


iterationDoAbout :: Iteration' RunWorld
iterationDoAbout =
    makeIteration' $ \run -> do
        gd <- resourceGUIData

        iteration' (iterationAboutFadeIn $ makeWidget gd) run

    where
      makeWidget gd = 
          (makeButtonWidget gd $ makeScreenWidget gd 0.5 0.5 
                               $ (makeButtonWidget gd
                               $ makeBorderWidgetSize gd valueGUIBorderSize
                               -- fixme: fill max
                               $ makeContourWidgetSize gd valueGUIContourSize
                               $ makeScrollWidgetHth gd (valueGUIHth)
                               $ makeTextWidget gd text
          ) `onPressA` (const AEmpty)) `onPressA` (const AEscape)


--------------------------------------------------------------------------------
--  

iterationAboutFadeIn :: WidgetT -> Iteration' RunWorld
iterationAboutFadeIn widget = 
    makeIteration' $ \run -> do
        
        let tick = worldTick run

        iteration' (iterationAboutFadeIn' tick widget) run



iterationAboutFadeIn' :: Tick -> WidgetT -> Iteration' RunWorld
iterationAboutFadeIn' tick w =
    mainIteration w outputMain'  $ defaultStep doEmpty $ \w run b -> do

        let alpha = rTF (worldTick run - tick) * valueRunAboutSettingsTicksInv

        (w', a') <- iterateGUINoInput alpha w AEmpty

        -- fade complete => do About
        if 1.0 <= alpha

          then return (run, b, [iterationAbout w'])

          else return (run, b, [iterationAboutFadeIn' tick w'])



iterationAbout :: WidgetT -> Iteration' RunWorld
iterationAbout w = 
    makeIteration' $ \run -> do

        -- set Camera
        let run' = setCamera run

        iteration' (iterationAbout' w) run'

    where
      setCamera run = 
          runSetCameraCmds run [] 


iterationAbout' :: WidgetT -> Iteration' RunWorld
iterationAbout' w =
    mainIteration w outputMain' $ defaultStep doEmpty $ \w run b -> do

        run' <- setCamera run

        run'' <- setMessage run'

        -- iterate GUI
        (w', a') <- iterateGUI 1.0 w AEmpty
        case a' of
            AEmpty    -> return (run'', b, [iterationAbout' w'])
            AEscape   -> return (run'', b, [iterationAboutFadeOut w'])
        
    where
      setCamera run = 
          if runCameraCmdsIsComplete run
            then do
              turn0 <- randomTurn
              view0 <- randomView
              return $ runSetCameraCmds run [ camcmdTurnAddView t0 s0 turn0 s0 view0 ]
            else return run

      randomTurn = io $ do
          randomRIO (0 :: UInt, 3 :: UInt) >>= \ix -> case ix of
              0   -> return leftTurn
              1   -> return rightTurn
              2   -> return downTurn
              3   -> return upTurn
              _   -> return straightTurn

      randomView = io $ do
          a <- randomIO >>= \uni -> return (smooth (-0.8) (0.8) uni)
          b <- randomIO >>= \uni -> return (smooth (-0.8) (0.8) uni)
          c <- randomIO >>= \uni -> return (smooth (valueRunBoundingRadius) (196.0) uni)
          return $ View a b c
      
      setMessage run = 
          if runMessageIsComplete run
            then commentAbout run
            else return run

      t0 = 2.0
      s0 = 1.0 / rTF t0


iterationAboutFadeOut :: WidgetT -> Iteration' RunWorld
iterationAboutFadeOut widget = 
    makeIteration' $ \run -> do
       
        -- stop camera movement
        let tick = worldTick run
            turn = cameraTurnIdeal $ runCamera run
            run' = run { runTurn = turn }
        
        -- clear message
        let run'' = setMessage run'

        iteration' (iterationAboutFadeOut' tick widget) run''
    
    where
      setMessage run =
          runMessageClear run
          


iterationAboutFadeOut' :: Tick -> WidgetT -> Iteration' RunWorld
iterationAboutFadeOut' tick w =
    mainIteration w outputMain' $ defaultStep doEmpty $ \w run b -> do

        let alpha = rTF (worldTick run - tick) * valueRunAboutSettingsTicksInv
        (w', a') <- iterateGUINoInput (1.0 - alpha) w AEmpty

        if 1.0 <= alpha
          then return (run, b, [])

          else return (run, b, [iterationAboutFadeOut' tick w'])


