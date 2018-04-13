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
module Game.Run.Output.Plain.Screen
  (
    outputScreenBegin,
    outputScreenBegin',
    outputScreenMain,
    outputScreenMain',
    outputScreenEnd,
    outputScreenEnd',
    
    outputScreenLevelMode,
    outputScreenPuzzleMode,
    outputScreenMemoryMode,
    outputScreenForeign,

    outputScreenAtFace',
    outputScreenAtFace''',
    outputScreenAToB,


  ) where


import MyPrelude
import Game

import Game.Font
import Game.Data.Color
import Game.Grid
import Game.Grid.Output
import Game.Run.RunData
import Game.Run.RunWorld
import Game.Run.RunWorld.OutputState
import Game.Run.Helpers
import Game.Run.Iteration.State
import Game.Run.Output.Plain.ShadeCube
import Game.Run.Scene.Plain.ShadeCorners

import OpenGL
import OpenGL.Helpers


--------------------------------------------------------------------------------
--  Begin


outputScreenBegin :: GameData -> RunWorld -> IO ()
outputScreenBegin griddata run = do
    return ()


outputScreenBegin' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                      s -> RunWorld -> b -> IO ()
outputScreenBegin' =
    outputScreenMain' 


--------------------------------------------------------------------------------
--  Main

outputScreenMain :: GameData -> RunWorld -> IO ()
outputScreenMain gamedata run =
    return ()
    
outputScreenMain' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                     s -> RunWorld -> b -> IO ()
outputScreenMain' gamedata proj modv normal s run b = do
    -- 3D --
    let projmodv = proj `mappend` modv
    glEnable gl_DEPTH_TEST
    glEnable gl_CULL_FACE

    -- space box
    let colormap = griddataColorMap $ gamedataGridData gamedata
        ix = outputstateColorIx $ runOutputState run 
        ix' = outputstateColorIx' $ runOutputState run 
        alpha = outputstateAlpha $ runOutputState run
    shadeSpaceBoxColor (griddataShadeSpaceBox $ gamedataGridData gamedata)
                       1.0 projmodv proj valuePerspectiveRadius $ 
                       smoothColor (colormapAt colormap ix) (colormapAt colormap ix') alpha
    
    -- cube
    shadeCube (rundataShadeCube $ gamedataRunData gamedata)
              1.0 projmodv normal run

    -- face names
    drawFaceNames gamedata 1.0 projmodv

    -- message path
    shadePathFatBegin (griddataShadePath $ gamedataGridData gamedata)
                      1.0 projmodv
    shadePathRadius (griddataShadePath $ gamedataGridData gamedata)
                    valueRunPathRadius
    shadePathColor (griddataShadePath $ gamedataGridData gamedata)
                   valueRunPathColor
    shadePathDrawPath0 (griddataShadePath $ gamedataGridData gamedata)
                       (gridPath $ runGrid run)
    shadePathEnd (griddataShadePath $ gamedataGridData gamedata)




--------------------------------------------------------------------------------
--  End

outputScreenEnd :: GameData -> RunWorld -> IO ()
outputScreenEnd gamedata run = 
    return ()

outputScreenEnd' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                    s -> RunWorld -> b -> IO ()
outputScreenEnd' gamedata proj modv normal s run b = do
    return ()



--------------------------------------------------------------------------------
--  Faces

outputScreenLevelMode :: GameData -> RunWorld -> IO ()
outputScreenLevelMode gamedata run = do
    return ()


outputScreenPuzzleMode :: GameData -> RunWorld -> IO ()
outputScreenPuzzleMode gamedata run = do
    return ()


outputScreenMemoryMode :: GameData -> RunWorld -> IO ()
outputScreenMemoryMode gamedata run = do
    return ()


outputScreenForeign :: GameData -> RunWorld -> IO ()
outputScreenForeign griddata run = do
    return ()
    
   

--------------------------------------------------------------------------------
--  AtFace

outputScreenAtFace' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                       AtFaceState -> RunWorld -> b -> IO ()
outputScreenAtFace' gamedata proj modv normal s run b = do

    -- 3D --
    let projmodv = proj `mappend` modv
    glEnable gl_DEPTH_TEST
    glEnable gl_CULL_FACE

    -- space box
    let colormap = griddataColorMap $ gamedataGridData gamedata
        ix = outputstateColorIx $ runOutputState run 
        ix' = outputstateColorIx' $ runOutputState run 
        alpha = outputstateAlpha $ runOutputState run
    shadeSpaceBoxColor (griddataShadeSpaceBox $ gamedataGridData gamedata)
                       1.0 projmodv proj valuePerspectiveRadius $ 
                       smoothColor (colormapAt colormap ix) (colormapAt colormap ix') alpha
    
    
    -- cube
    shadeCube (rundataShadeCube $ gamedataRunData gamedata)
              1.0 projmodv normal run

    -- face names
    let alpha = cameraViewAlpha $ runCamera run
    drawFaceNames gamedata (1.0 - alpha) projmodv

    -- message path
    shadePathFatBegin (griddataShadePath $ gamedataGridData gamedata)
                      1.0 projmodv
    shadePathRadius (griddataShadePath $ gamedataGridData gamedata)
                    valueRunPathRadius
    shadePathColor (griddataShadePath $ gamedataGridData gamedata)
                   valueRunPathColor
    shadePathDrawPath0 (griddataShadePath $ gamedataGridData gamedata)
                       (gridPath $ runGrid run)
    shadePathEnd (griddataShadePath $ gamedataGridData gamedata)

    -- escape corners
    drawAtFaceCorners gamedata alpha projmodv s run




outputScreenAtFace''' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                         AtFaceState -> RunWorld -> b -> IO ()
outputScreenAtFace''' gamedata proj modv normal s run b = do

    -- 3D --
    let projmodv = proj `mappend` modv
    glEnable gl_DEPTH_TEST
    glEnable gl_CULL_FACE

    -- space box
    let colormap = griddataColorMap $ gamedataGridData gamedata
        ix = outputstateColorIx $ runOutputState run 
        ix' = outputstateColorIx' $ runOutputState run 
        alpha = outputstateAlpha $ runOutputState run
    shadeSpaceBoxColor (griddataShadeSpaceBox $ gamedataGridData gamedata)
                       1.0 projmodv proj valuePerspectiveRadius $ 
                       smoothColor (colormapAt colormap ix) (colormapAt colormap ix') alpha
    
    -- cube
    shadeCube (rundataShadeCube $ gamedataRunData gamedata)
              1.0 projmodv normal run

    -- face names
    let alpha = cameraViewAlpha $ runCamera run
    drawFaceNames gamedata alpha projmodv

    -- message path
    shadePathFatBegin (griddataShadePath $ gamedataGridData gamedata)
                      1.0 projmodv
    shadePathRadius (griddataShadePath $ gamedataGridData gamedata)
                    valueRunPathRadius
    shadePathColor (griddataShadePath $ gamedataGridData gamedata)
                   valueRunPathColor
    shadePathDrawPath0 (griddataShadePath $ gamedataGridData gamedata)
                       (gridPath $ runGrid run)
    shadePathEnd (griddataShadePath $ gamedataGridData gamedata)
    
    -- escape corners
    drawAtFaceCorners gamedata (1.0 - alpha) projmodv s run




-- | iterationMain' withouth face names
outputScreenAToB :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                    s -> RunWorld -> b -> IO ()
outputScreenAToB gamedata proj modv normal s run b = do
    -- 3D --
    let projmodv = proj `mappend` modv
    glEnable gl_DEPTH_TEST
    glEnable gl_CULL_FACE

    -- space box
    let colormap = griddataColorMap $ gamedataGridData gamedata
        ix = outputstateColorIx $ runOutputState run 
        ix' = outputstateColorIx' $ runOutputState run 
        alpha = outputstateAlpha $ runOutputState run
    shadeSpaceBoxColor (griddataShadeSpaceBox $ gamedataGridData gamedata)
                       1.0 projmodv proj valuePerspectiveRadius $ 
                       smoothColor (colormapAt colormap ix) (colormapAt colormap ix') alpha
    
    -- cube
    shadeCube (rundataShadeCube $ gamedataRunData gamedata)
              1.0 projmodv normal run

    -- message path
    shadePathFatBegin (griddataShadePath $ gamedataGridData gamedata)
                      1.0 projmodv
    shadePathRadius (griddataShadePath $ gamedataGridData gamedata)
                    valueRunPathRadius
    shadePathColor (griddataShadePath $ gamedataGridData gamedata)
                   valueRunPathColor
    shadePathDrawPath0 (griddataShadePath $ gamedataGridData gamedata)
                       (gridPath $ runGrid run)
    shadePathEnd (griddataShadePath $ gamedataGridData gamedata)



--------------------------------------------------------------------------------
--  


drawFaceNames :: GameData -> Float -> Mat4 -> IO ()
drawFaceNames gamedata alpha projmodv = do
    let fsh = gamedataFontShade gamedata        
        ffd = gamedataFontData gamedata
        r = fI valueRunCubeRadius
    fontShade fsh alpha projmodv         
    fontDrawDefault fsh ffd valueRunCubeFontSize valueRunCubeFontColor
    fontSetPlane3D fsh 0 0 1 0 1 0
    fontDraw3DCentered fsh ffd (-r) 0 0 "LevelMode"
    --fontSetPlane3D fsh 1 0 0 0 1 0
    --fontDraw3DCentered fsh ffd 0 0 (r)  "Foreign"
    fontSetPlane3D fsh 0 0 (-1) 0 1 0
    fontDraw3DCentered fsh ffd (r) 0 0  "PuzzleMode"
    fontSetPlane3D fsh (-1) 0 0 0 1 0
    fontDraw3DCentered fsh ffd 0 0 (-r) "MemoryMode"



drawAtFaceCorners :: GameData -> Float -> Mat4 -> AtFaceState -> RunWorld -> IO ()
drawAtFaceCorners gamedata alpha projmodv s run = do
    let Shape wth hth = sceneShape $ runScene run
        wth' = fI valueRunCubeRadius * wth
        hth' = fI valueRunCubeRadius * hth
        r = fI valueRunCubeRadius * 1.001
    case atfacestateFace s of
        FaceLevelMode   -> 
            shadeCorners3D (rundataShadeCorners $ gamedataRunData gamedata) alpha projmodv
                           (-r) (-hth') (-wth')
                           (hth * valueSceneCornerSize) 0.0 0.0 (2.0 * wth')
                           (wth * valueSceneCornerSize) 0.0 (2.0 * hth') 0.0

        FacePuzzleMode -> 
            shadeCorners3D (rundataShadeCorners $ gamedataRunData gamedata) alpha projmodv
                           (r) (-hth') (wth')
                           (hth * valueSceneCornerSize) 0.0 0.0 (-2.0 * wth')
                           (wth * valueSceneCornerSize) 0.0 (2.0 * hth') 0.0
        
        FaceMemoryMode -> 
            shadeCorners3D (rundataShadeCorners $ gamedataRunData gamedata) alpha projmodv
                           (wth') (-hth') (-r)
                           (hth * valueSceneCornerSize) ((-2.0) * wth') 0.0 0.0
                           (wth * valueSceneCornerSize) 0.0 (2.0 * hth') 0.0

        _              -> 
            return ()

