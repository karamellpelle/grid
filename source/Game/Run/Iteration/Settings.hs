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
module Game.Run.Iteration.Settings
  (
    iterationDoSettings,

  ) where


import MyPrelude
import Game

import Game.Grid.Helpers
import Game.Run.RunWorld
import Game.Run.Do
import Game.Run.Output
import Game.Run.Helpers
import Game.Run.Helpers.World
import Game.Run.Helpers.Make
import Game.Run.Iteration.Iteration

import Game.GUI
import Game.GUI.Widget.ScreenWidget
import Game.GUI.Widget.BorderWidget
import Game.GUI.Widget.LayoutWidget
import Game.GUI.Widget.SlideWidget
import Game.GUI.Widget.NumberWidget
import Game.GUI.Widget.LabelWidget
import Game.GUI.Widget.ButtonWidget



data WidgetA =
    AEmpty                    |
    ASetIntensity  !Float     |
    ASetLevelPuzzle !UInt     |
    ASetMemory !UInt          |
    AEscape
   
type WidgetT =
    ButtonWidget WidgetA


data SetSettings =
    SetSettings
    {
        setIntensity :: !Float,
        setLevelPuzzle :: !UInt,
        setMemory :: !UInt
    }



makeSetSettings :: Float -> UInt -> UInt -> SetSettings
makeSetSettings intens lix mix =
    SetSettings
    {
        setIntensity = intens,
        setLevelPuzzle = lix,
        setMemory = mix
    }



iterationDoSettings :: Iteration' RunWorld
iterationDoSettings =
    makeIteration' $ \run -> do
        gd <- resourceGUIData
        iteration' (iterationSettingsFadeIn $ makeWidget gd run) run

    where
      makeWidget gd run = 
          (makeButtonWidget gd $ makeScreenWidget gd 0.5 0.5
                               $ (makeButtonWidget gd 
                               $ makeBorderWidgetSize gd valueGUIBorderSize
                               $ makeLayoutWidgetStatic gd
                               $ xgatherFill1 valueGUIHth 
                               $ ygatherLeftFill4 valueGUIWth
                                 (
                                    yfill 0.05 $ xfill valueGUIWth $ widget $  
                                        makeLabelWidget gd "* SETTINGS *"
                                 )
                                 (
                                    makeIntensityW gd (runIntensity run)
                                 )
                                 (
                                    makeLevelPuzzleW gd (runLevelPuzzlePeak run) 
                                                        (runLevelPuzzleIx run)
                                 )
                                 (
                                    makeMemoryW gd (runMemoryPeak run)
                                                   (runMemoryIx run)
                                 )
          ) `onPressA` (const AEmpty)) `onPressA` (const AEscape)

      makeIntensityW gd value = 
          yfillTop 0.09 $ ygatherLeft2 
              (widget $ makeLabelWidget gd " Intensity")
              (widget $ (makeSlideWidget gd 0.6 0.0 value)
                         `onNewValue` (\value _ -> ASetIntensity value))

      makeLevelPuzzleW gd peak ix = 
         yfillTop 0.09 $ xapart 0.5
               (widget $ makeLabelWidget gd " LevelPuzzle")
               (widget $ (makeNumberWidget gd 0 (fI peak) (fI ix))
                         `onNewNumber` (\ix _ -> ASetLevelPuzzle (fI ix)))

      makeMemoryW gd peak ix = 
         yfillTop 0.09 $ xapart 0.5
               (widget $ makeLabelWidget gd " Memory")
               (widget $ (makeNumberWidget gd 0 (fI peak) (fI ix))
                         `onNewNumber` (\ix _ -> ASetMemory (fI ix)))

--------------------------------------------------------------------------------
--  


iterationSettingsFadeIn :: WidgetT -> Iteration' RunWorld
iterationSettingsFadeIn widget = 
    makeIteration' $ \run -> do
        
        let tick = worldTick run
            run' = runSetCameraCmds run [ camcmdTurnAddView t0 s0 turn0 s0 view0 ]

        iteration' (iterationSettingsFadeIn' tick widget) run'

    where
      t0 = 2.0
      s0 = 1.0 / rTF t0
      turn0 = upTurn
      view0 = View 0.0 0.0 64.0


iterationSettingsFadeIn' :: Tick -> WidgetT -> Iteration' RunWorld
iterationSettingsFadeIn' tick w =
    mainIteration w outputMain' $ defaultStep doEmpty $ \w run b -> do

        let alpha = rTF (worldTick run - tick) * valueRunAboutSettingsTicksInv
        
        (w', a') <- iterateGUINoInput alpha w AEmpty

        -- fade complete => do Settings
        if 1.0 <= alpha
          then return (run, b, [iterationSettings w'])
          else return (run, b, [iterationSettingsFadeIn' tick w'])



iterationSettings :: WidgetT -> Iteration' RunWorld
iterationSettings widget = 
    makeIteration' $ \run -> do
       
        let run' = setCamera run
            set = makeSetSettings (runIntensity run) 
                                  (runLevelPuzzleIx run)
                                  (runMemoryIx run)

        iteration' (iterationSettings' set widget) run'

    where
      setCamera run =
          runSetCameraCmds run []
      




iterationSettings' :: SetSettings -> WidgetT -> Iteration' RunWorld
iterationSettings' set w =
    mainIteration w outputMain' $ defaultStep doEmpty $ \w run b -> do

        run' <- setCamera run 

        -- iterate GUI
        (w', a') <- iterateGUI 1.0 w AEmpty

        case a' of
            
            ASetIntensity alpha   -> do
                gameSetIntensity alpha
                let set' = set { setIntensity = alpha }
                return (run', b, [iterationSettings' set' w'])

            ASetLevelPuzzle ix    -> do
                let set' = set { setLevelPuzzle = ix }
                return (run', b, [iterationSettings' set' w'])

            -- set memory ix, restart world
            ASetMemory ix         -> do
                let set' = set { setMemory = ix } 
                return (run', b, [iterationSettings' set' w'])

            -- finish Settings
            AEscape               -> do

                -- update to new settings
                run'' <- updateIntensity set run'
                run''' <- updateLevelPuzzle set run''
                run'''' <- updateMemory set run'''
               
                -- save
                saveRunWorld run''''

                return (run'''', b, [iterationSettingsFadeOut w'])

            _                     -> do
                return (run', b, [iterationSettings' set w'])


    where
      setCamera run = 
          if runCameraCmdsIsComplete run
            then return $ runSetCameraCmds run [ camcmdTurnAdd t0 s0 turn0 ]
            else return run

      t0 = 2.0
      s0 = 1.0 / rTF t0
      turn0 = leftTurn

      updateLevelPuzzle set run = 
          if runLevelPuzzleIx run == setLevelPuzzle set
            then return run
            else runLevelPuzzleSetIx run (setLevelPuzzle set)

      updateMemory set run = 
          if runMemoryIx run == setMemory set
            then return run
            else runMemorySetIx run (setMemory set)
                          
      updateIntensity set run = 
          return run { runIntensity = setIntensity set }



iterationSettingsFadeOut :: WidgetT -> Iteration' RunWorld
iterationSettingsFadeOut w = 
    makeIteration' $ \run -> do
        
        let tick = worldTick run
            turn = cameraTurnIdeal $ runCamera run
            run' = run { runTurn = turn } 
        iteration' (iterationSettingsFadeOut' tick w) run'
    

iterationSettingsFadeOut' :: Tick -> WidgetT -> Iteration' RunWorld
iterationSettingsFadeOut' tick w =
    mainIteration w outputMain' $ defaultStep doEmpty $ \w run b -> do

        let alpha = rTF (worldTick run - tick) * valueRunAboutSettingsTicksInv

        (w', a') <- iterateGUINoInput (1.0 - alpha) w AEmpty

        if 1.0 <= alpha
          then return (run, b, [])

          else return (run, b, [iterationSettingsFadeOut' tick w'])

