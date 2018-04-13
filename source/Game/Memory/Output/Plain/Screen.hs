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
module Game.Memory.Output.Plain.Screen
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
outputScreenShow' gamedata proj modv normal s mem run = do

    -- 3D --
    let projmodv = proj `mappend` modv
    glEnable gl_DEPTH_TEST
    glEnable gl_CULL_FACE

    -- space box
    let colormap = griddataColorMap $ gamedataGridData gamedata
        ix = outputstateColorIx $ memoryOutputState mem
        ix' = outputstateColorIx' $ memoryOutputState mem
        alpha = outputstateAlpha $ memoryOutputState mem
    shadeSpaceBoxColor (griddataShadeSpaceBox $ gamedataGridData gamedata)
                       1.0 projmodv proj valuePerspectiveRadius $ 
                       colormapAt colormap ix'
                       --smoothColor (colormapAt colormap ix)
                       --            (colormapAt colormap ix') 
                       --            alpha

    -- message path
    shadePathBegin (griddataShadePath $ gamedataGridData gamedata)
                   1.0 projmodv
    shadePathRadius (griddataShadePath $ gamedataGridData gamedata)
                    valueMemoryPathRadius
    shadePathColor (griddataShadePath $ gamedataGridData gamedata)
                   (Color 0.0 1.0 0.0 1.0)
    shadePathDraw (griddataShadePath $ gamedataGridData gamedata) $ 
                  gridPath $ memoryGrid mem
    shadePathEnd (griddataShadePath $ gamedataGridData gamedata)


    -- 2D --
    let Shape wth hth = sceneShape $ runScene run
        projmodv = mat4Ortho 0 wth hth 0 (-1) 1
    glDisable gl_CULL_FACE

    -- TextLevel
    drawTextLevel gamedata projmodv mem run




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
outputScreenPlay' gamedata proj modv normal s mem run = do

    -- 3D --
    let projmodv = proj `mappend` modv
    glEnable gl_DEPTH_TEST
    glEnable gl_CULL_FACE


    -- space box
    let colormap = griddataColorMap $ gamedataGridData gamedata
        ix = outputstateColorIx $ memoryOutputState mem
        ix' = outputstateColorIx' $ memoryOutputState mem
        alpha = outputstateAlpha $ memoryOutputState mem
    shadeSpaceBoxColor (griddataShadeSpaceBox $ gamedataGridData gamedata)
                       1.0 projmodv proj valuePerspectiveRadius $ 
                       colormapAt colormap ix'
                       --smoothColor (colormapAt colormap ix)
                       --            (colormapAt colormap ix') 
                       --            alpha
    
    -- message path
    shadePathBegin (griddataShadePath $ gamedataGridData gamedata)
                   1.0 projmodv
    shadePathRadius (griddataShadePath $ gamedataGridData gamedata)
                    valueMemoryPathRadius
    shadePathColor (griddataShadePath $ gamedataGridData gamedata)
                   (Color 1.0 1.0 0.0 1.0) 
    shadePathDraw (griddataShadePath $ gamedataGridData gamedata) $ 
                  gridPath $ memoryGrid mem
    shadePathEnd (griddataShadePath $ gamedataGridData gamedata)


    -- 2D --
    let Shape wth hth = sceneShape $ runScene run
        projmodv = mat4Ortho 0 wth hth 0 (-1) 1
    glDisable gl_CULL_FACE







--------------------------------------------------------------------------------
--  Failure

outputScreenFailure :: GameData -> MemoryWorld -> RunWorld -> IO ()
outputScreenFailure gamedata mem run = do
    return ()


outputScreenFailure' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                        s -> MemoryWorld -> RunWorld -> IO ()
outputScreenFailure' gamedata proj modv normal tick mem run = do
    
    -- 3D --
    let projmodv = proj `mappend` modv
    glEnable gl_DEPTH_TEST
    glEnable gl_CULL_FACE


    -- space box
    let colormap = griddataColorMap $ gamedataGridData gamedata
        ix = outputstateColorIx $ memoryOutputState mem
        ix' = outputstateColorIx' $ memoryOutputState mem
        alpha = outputstateAlpha $ memoryOutputState mem
    shadeSpaceBoxColor (griddataShadeSpaceBox $ gamedataGridData gamedata)
                       1.0 projmodv proj valuePerspectiveRadius $ 
                       colormapAt colormap ix'
                       --smoothColor (colormapAt colormap ix)
                       --            (colormapAt colormap ix') 
                       --            alpha
    
    -- shadePath --
    let path = gridPath $ memoryGrid mem
        size = pathSize path
        alpha = pathAlpha path
        ix = memoryFailureIx mem
    shadePathBegin (griddataShadePath $ gamedataGridData gamedata)
                   1.0 projmodv
    shadePathRadius (griddataShadePath $ gamedataGridData gamedata) 
                    valueMemoryPathRadius
    -- path OK
    shadePathColor (griddataShadePath $ gamedataGridData gamedata)
                   (Color 1.0 1.0 0.0 1.0)
    shadePathDrawPath0BeginEnd (griddataShadePath $ gamedataGridData gamedata)
                               0 ix path
    -- path correct
    shadePathColor (griddataShadePath $ gamedataGridData gamedata)
                   (Color 0.0 1.0 0.0 1.0)
    shadePathDrawBeginEnd (griddataShadePath $ gamedataGridData gamedata)
                          ix size alpha path
    shadePathEnd (griddataShadePath $ gamedataGridData gamedata)
    
    -- path failure
    let alpha' = if size == ix then alpha else 1.0
    shadePathAlpha (griddataShadePath $ gamedataGridData gamedata) $ 
                   1.0 - alpha'
    shadePathColor (griddataShadePath $ gamedataGridData gamedata)
                   (Color 1.0 0.0 0.0 1.0)
    shadePathDrawSegment (griddataShadePath $ gamedataGridData gamedata)
                         (memoryFailureSegment mem) alpha'
  

    -- 2D --
    let Shape wth hth = sceneShape $ runScene run
        projmodv = mat4Ortho 0 wth hth 0 (-1) 1
    glDisable gl_CULL_FACE
    
    -- TextFailure
    drawTextFailure gamedata projmodv mem run










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
