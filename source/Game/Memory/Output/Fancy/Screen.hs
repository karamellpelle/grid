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
module Game.Memory.Output.Fancy.Screen
  (
    outputScreenBeginShow,
    outputScreenShow,
    outputScreenShow',
    outputScreenBeginPlay,
    outputScreenPlay,
    outputScreenPlay',
    outputScreenFailure,
    outputScreenFailure',

  ) where

import MyPrelude
import Game

import Game.Font
import Game.Data.Color
import Game.Grid
import Game.Grid.Output
import Game.Memory
import Game.Memory.MemoryWorld.OutputState
import Game.Run.RunWorld

import OpenGL
import OpenGL.Helpers




--------------------------------------------------------------------------------
--  Show

outputScreenBeginShow :: GameData -> MemoryWorld -> RunWorld -> IO ()
outputScreenBeginShow gamedata mem run = do
    return ()


outputScreenShow :: GameData -> MemoryWorld -> RunWorld -> IO ()
outputScreenShow gamedata mem run = do
    return ()


outputScreenShow' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                     s -> MemoryWorld -> RunWorld -> IO ()
outputScreenShow' gamedata proj2D proj3D modv3D = \s mem run -> do

    -- 3D --
    let modv = modv3D 
        projmodv = proj3D `mappend` modv
        normal = modv

    -- space box
    let sh = griddataShadeSpace $ gamedataGridData gamedata
        colormap = griddataColorMap $ gamedataGridData gamedata
        ix0 = ostateColorIx0 $ memoryOutputState mem
        ix1 = ostateColorIx1 $ memoryOutputState mem
        tweak = sceneTweak $ runScene run
        alpha = fade fadeTicksInv 0.0 (worldTick mem)
        color = smoothColor (colormapAt colormap ix0) (colormapAt colormap ix1) alpha
        refmodv = refModv mem modv 
    shadeSpaceColor sh tweak 1.0 refmodv color 

    -- message path
    let sh = griddataShadePath $ gamedataGridData gamedata 
    shadePathBegin sh 1.0 projmodv
    shadePathRadius sh valueMemoryPathRadius
    shadePathColor sh colorGreen
    shadePathDraw sh (gridPath $ memoryGrid mem)
    shadePathEnd sh


    -- 2D --
    let projmodv = proj2D
    glDisable gl_CULL_FACE

    -- TextLevel
    drawTextLevel gamedata projmodv mem run

    glEnable gl_CULL_FACE

    where 
      fadeTicks = 1.0
      fadeTicksInv = 1.0 / fadeTicks


--------------------------------------------------------------------------------
--  Play


outputScreenBeginPlay :: GameData -> MemoryWorld -> RunWorld -> IO ()
outputScreenBeginPlay gamedata mem run = do
    return ()


outputScreenPlay :: GameData -> MemoryWorld -> RunWorld -> IO ()
outputScreenPlay gamedata mem run = do
    return ()


outputScreenPlay' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                     s -> MemoryWorld -> RunWorld -> IO ()
outputScreenPlay' gamedata proj2D proj3D modv3D = \s mem run -> do

    -- 3D --
    let modv = modv3D
        projmodv = proj3D `mappend` modv
        normal = modv

    -- space box
    let sh = griddataShadeSpace $ gamedataGridData gamedata
        colormap = griddataColorMap $ gamedataGridData gamedata
        ix0 = ostateColorIx0 $ memoryOutputState mem
        ix1 = ostateColorIx1 $ memoryOutputState mem
        color = colormapAt colormap ix1
        tweak = sceneTweak $ runScene run
        refmodv = refModv mem modv 
    shadeSpaceColor sh tweak 1.0 refmodv color 
  

    -- message path
    let sh = griddataShadePath $ gamedataGridData gamedata
    shadePathBegin sh 1.0 projmodv
    shadePathRadius sh valueMemoryPathRadius
    shadePathColor sh colorYellow
    shadePathDraw sh (gridPath $ memoryGrid mem)
    shadePathEnd sh


    -- 2D --
    --let projmodv = proj2D
    --glDisable gl_CULL_FACE
    --glEnable gl_CULL_FACE



--------------------------------------------------------------------------------
--  Failure

outputScreenFailure :: GameData -> MemoryWorld -> RunWorld -> IO ()
outputScreenFailure gamedata mem run = do
    return ()


outputScreenFailure' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                        s -> MemoryWorld -> RunWorld -> IO ()
outputScreenFailure' gamedata proj2D proj3D modv3D = \tick mem run -> do
    
    -- 3D --
    let modv = modv3D
        projmodv = proj3D `mappend` modv
        normal = modv

    -- space box
    let sh = griddataShadeSpace $ gamedataGridData gamedata
        colormap = griddataColorMap $ gamedataGridData gamedata
        ix0 = ostateColorIx0 $ memoryOutputState mem
        ix1 = ostateColorIx1 $ memoryOutputState mem
        fadeSpace = fade fadeSpaceTicksInv (memoryFailureTick mem) (worldTick mem)    
        color = smoothColor (colormapAt colormap ix1) colorNull fadeSpace
        tweak = sceneTweak $ runScene run
        refmodv = refModv mem modv 
    shadeSpaceColor sh tweak 1.0 refmodv color 
    
    -- shadePath --
    -- fixme: assert 'ix', 'size', ... are correct
    let sh = griddataShadePath $ gamedataGridData gamedata
        path = gridPath $ memoryGrid mem
        size = pathSize path
        ix = memoryFailureIx mem
        alpha = pathAlpha path
    shadePathBegin sh 1.0 projmodv
    shadePathRadius sh valueMemoryPathRadius

    -- path OK
    shadePathColor sh colorYellow
    shadePathDrawPath0BeginEnd sh 0 ix path

    -- path failure
    let alpha' = if size == ix then alpha else 1.0
    shadePathColor sh colorRed
    shadePathDrawSegment sh (memoryFailureSegment mem) alpha'
  
    -- path correct
    let fadeCorrect = smooth 1.0 fadeCorrectTrans $ 
                      fade fadeCorrectTicksInv (memoryFailureTick mem) (worldTick mem)
    shadePathAlpha sh fadeCorrect
    shadePathColor sh colorGreen
    shadePathDrawBeginEnd sh ix size alpha path
    shadePathEnd sh
    

    -- 2D --
    let projmodv = proj2D
    glDisable gl_CULL_FACE
    
    -- TextFailure
    drawTextFailure gamedata projmodv mem run
    
    glEnable gl_CULL_FACE

    where
      fadeSpaceTicks = 8.0
      fadeSpaceTicksInv = 1.0 / fadeSpaceTicks
      fadeCorrectTicks = 2.0
      fadeCorrectTicksInv = 1.0 / fadeCorrectTicks
      fadeCorrectTrans = 0.4








--------------------------------------------------------------------------------
---  

drawTextLevel:: GameData -> Mat4 -> MemoryWorld -> RunWorld -> IO ()
drawTextLevel gamedata projmodv mem run = do
    let fadeText = min valueLevelTrans $ 
                   fade valueLevelTicksInv 0.0 (worldTick mem)
    let fsh = gamedataFontShade gamedata        
        ffd = gamedataFontData gamedata
        Shape wth hth = sceneShape $ runScene run
    fontShade fsh (1.0 - fadeText) projmodv         

    fontDrawDefault fsh ffd (valueTextFontASize * hth) valueTextFontAColor
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueTextFontAY * hth) $ 
                       "level " ++ show (memoryLevelIx mem)

    fontDrawDefault fsh ffd (valueTextFontBSize * hth) valueTextFontBColor
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueTextFontBY * hth) $ 
                       "(size: " ++ show (memoryLevelSize mem) ++ ")"

    where
      valueLevelTicks     = 4.0
      valueLevelTicksInv  = 1.0 / valueLevelTicks
      valueLevelTrans     = 0.9



drawTextFailure :: GameData -> Mat4 -> MemoryWorld -> RunWorld -> IO ()
drawTextFailure gamedata projmodv mem run= do
    let fadeText = min valueFailureTrans $ 
                   fade valueFailureTicksInv (memoryFailureTick mem) (worldTick mem)
    let fsh = gamedataFontShade gamedata        
        ffd = gamedataFontData gamedata
        Shape wth hth = sceneShape $ runScene run 
    fontShade fsh (1.0 - fadeText) projmodv         

    fontDrawDefault fsh ffd (valueTextFontASize * hth) $ valueTextFontAColor
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueTextFontAY * hth) $
                       "failure..."
    
    fontSetAlpha fsh 1.0
    fontDrawDefault fsh ffd (valueTextFontCSize * hth) valueTextFontCColor
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueTextFontCY * hth) $ 
                       "(touch and hold to show)"
    
    where
      valueFailureTicks     = 8.0
      valueFailureTicksInv  = 1.0 / valueFailureTicks
      valueFailureTrans     = 0.9


--------------------------------------------------------------------------------
--  helpers

fade :: Float -> Tick -> Tick -> Float
fade scale t0 t1 = 
    min 1.0 $ scale * rTF (t1 - t0)


refModv :: MemoryWorld -> Mat4 -> Mat4
refModv mem mat = 
    mat `mappend` mat4Turn (memorySpaceRef mem)
