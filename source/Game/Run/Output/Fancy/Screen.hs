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
module Game.Run.Output.Fancy.Screen
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
    outputScreenKonami,

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
import Game.Grid.Output.Fancy.ShadeGeneral
import Game.Run
import Game.Run.Iteration.State
import Game.Run.Output.Fancy.ShadeCube
import Game.Run.Output.Fancy.CornerData
import Game.LevelPuzzle.LevelPuzzleWorld.OutputState

import OpenGL
import OpenGL.Helpers


--------------------------------------------------------------------------------
--  Begin


outputScreenBegin :: GameData -> RunWorld -> IO ()
outputScreenBegin griddata run = do
    return ()


outputScreenBegin' :: GameData -> Mat4 -> Mat4 -> Mat4 -> s -> RunWorld -> b -> IO ()
outputScreenBegin' gamedata proj2D proj3D modv3D = \s run b -> do
    -- 3D --
    let modv = modv3D
        projmodv = proj3D `mappend` modv
        normal = modv

    -- space box
    let sh = griddataShadeSpace $ gamedataGridData gamedata
        colormap = griddataColorMap $ gamedataGridData gamedata
        ix0 = ostateColorIx0 $ levelpuzzleOutputState $ runLevelPuzzleWorld run 
        ix1 = ostateColorIx1 $ levelpuzzleOutputState $ runLevelPuzzleWorld run 
        alpha = ostateAlpha $ levelpuzzleOutputState $ runLevelPuzzleWorld run
        tweak = sceneTweak $ runScene run
    --shadeSpaceColor sh tweak 1.0 modv $ smoothColor (colormapAt colormap ix0) 
    --                                                (colormapAt colormap ix1) alpha
    shadeSpace sh tweak 1.0 modv

    -- cube
    let sh = rundataShadeCube $ gamedataRunData gamedata
    shadeCube sh 1.0 projmodv normal 0.0 run

    -- face names
    drawFaceNamesBegin gamedata projmodv run

    -- message path
    let sh = griddataShadePath $ gamedataGridData gamedata
    shadePathBegin sh 1.0 projmodv
    shadePathRadius sh valueRunPathRadius
    shadePathColor sh valueRunPathColor
    shadePathDrawPath0 sh (gridPath $ runGrid run)
    shadePathEnd sh

      

--------------------------------------------------------------------------------
--  Main

outputScreenMain :: GameData -> RunWorld -> IO ()
outputScreenMain gamedata run =
    return ()
    
outputScreenMain' :: GameData -> Mat4 -> Mat4 -> Mat4 -> s -> RunWorld -> b -> IO ()
outputScreenMain' gamedata proj2D proj3D modv3D = \s run b -> do
    -- 3D --
    let modv = modv3D
        projmodv = proj3D `mappend` modv
        normal = modv

    -- space box
    let sh = griddataShadeSpace $ gamedataGridData gamedata
        colormap = griddataColorMap $ gamedataGridData gamedata
        ix0 = ostateColorIx0 $ levelpuzzleOutputState $ runLevelPuzzleWorld run 
        ix1 = ostateColorIx1 $ levelpuzzleOutputState $ runLevelPuzzleWorld run 
        alpha = ostateAlpha $ levelpuzzleOutputState $ runLevelPuzzleWorld run
        tweak = sceneTweak $ runScene run
    --shadeSpaceColor sh tweak 1.0 modv $ smoothColor (colormapAt colormap ix0) 
    --                                                (colormapAt colormap ix1) alpha
    shadeSpace sh tweak 1.0 modv

    -- cube
    let sh = rundataShadeCube $ gamedataRunData gamedata
        beta = 1.0 -- grayscale
    shadeCube sh 1.0 projmodv modv beta run

    -- face names
    drawFaceNames gamedata 1.0 projmodv

    -- message path
    let sh = griddataShadePath $ gamedataGridData gamedata
    shadePathBegin sh 1.0 projmodv
    shadePathRadius sh valueRunPathRadius
    shadePathColor sh valueRunPathColor
    shadePathDrawPath0 sh (gridPath $ runGrid run)
    shadePathEnd sh







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
    

outputScreenKonami :: GameData -> RunWorld -> IO ()
outputScreenKonami griddata run = do
    return ()
    
   

--------------------------------------------------------------------------------
--  AtFace

outputScreenAtFace' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                       AtFaceState -> RunWorld -> b -> IO ()
outputScreenAtFace' gamedata proj2D proj3D modv3D s run b = do

    -- 3D --
    let modv = modv3D
        projmodv = proj3D `mappend` modv
        normal = modv

    -- space box
    let sh = griddataShadeSpace $ gamedataGridData gamedata
        colormap = griddataColorMap $ gamedataGridData gamedata
        ix0 = ostateColorIx0 $ levelpuzzleOutputState $ runLevelPuzzleWorld run 
        ix1 = ostateColorIx1 $ levelpuzzleOutputState $ runLevelPuzzleWorld run 
        alpha = ostateAlpha $ levelpuzzleOutputState $ runLevelPuzzleWorld run
        tweak = sceneTweak $ runScene run
    --shadeSpaceColor sh tweak 1.0 modv $ smoothColor (colormapAt colormap ix0) 
    --                                                (colormapAt colormap ix1) alpha
    shadeSpace sh tweak 1.0 modv
    
    -- cube
    let sh = rundataShadeCube $ gamedataRunData gamedata
        a = cameraViewAlpha $ runCamera run
        beta = 1.0 - a * a
    shadeCube sh 1.0 projmodv modv beta run

    -- face names
    let alpha = cameraViewAlpha $ runCamera run
    drawFaceNames gamedata (1.0 - alpha) projmodv

    -- message path
    let sh = griddataShadePath $ gamedataGridData gamedata
    shadePathBegin sh 1.0 projmodv
    shadePathRadius sh valueRunPathRadius
    shadePathColor sh valueRunPathColor
    shadePathDrawPath0 sh (gridPath $ runGrid run)
    shadePathEnd sh

    -- escape corners
    --drawAtFaceCorners gamedata alpha projmodv s run



outputScreenAtFace''' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                         AtFaceState -> RunWorld -> b -> IO ()
outputScreenAtFace''' gamedata proj2D proj3D modv3D = \s run b -> do
    
    -- 3D --
    let modv = modv3D
        projmodv = proj3D `mappend` modv
        normal = modv

    -- space box
    let sh = griddataShadeSpace $ gamedataGridData gamedata
        colormap = griddataColorMap $ gamedataGridData gamedata
        ix0 = ostateColorIx0 $ levelpuzzleOutputState $ runLevelPuzzleWorld run 
        ix1 = ostateColorIx1 $ levelpuzzleOutputState $ runLevelPuzzleWorld run 
        alpha = ostateAlpha $ levelpuzzleOutputState $ runLevelPuzzleWorld run
        tweak = sceneTweak $ runScene run
    --shadeSpaceColor sh tweak 1.0 modv $ smoothColor (colormapAt colormap ix0) 
    --                                                (colormapAt colormap ix1) alpha
    shadeSpace sh tweak 1.0 modv


    -- cube
    let sh = rundataShadeCube $ gamedataRunData gamedata
        a = cameraViewAlpha $ runCamera run 
        beta = a * a
    shadeCube sh 1.0 projmodv modv beta run 

    -- face names
    let alpha = cameraViewAlpha $ runCamera run
    drawFaceNames gamedata alpha projmodv

    -- message path
    let sh = griddataShadePath $ gamedataGridData gamedata
    shadePathBegin sh 1.0 projmodv
    shadePathRadius sh valueRunPathRadius
    shadePathColor sh valueRunPathColor
    shadePathDrawPath0 sh (gridPath $ runGrid run)
    shadePathEnd sh
    
    -- escape corners
    --drawAtFaceCorners gamedata (1.0 - alpha) projmodv s run



-- | iterationMain' withouth face names
outputScreenAToB :: GameData -> Mat4 -> Mat4 -> Mat4 -> s -> RunWorld -> b -> IO ()
outputScreenAToB gamedata proj2D proj3D modv3D s run b = do
    -- 3D --
    let modv = modv3D
        projmodv = proj3D `mappend` modv
        normal = modv

    -- space box
    let sh = griddataShadeSpace $ gamedataGridData gamedata
        colormap = griddataColorMap $ gamedataGridData gamedata
        ix0 = ostateColorIx0 $ levelpuzzleOutputState $ runLevelPuzzleWorld run 
        ix1 = ostateColorIx1 $ levelpuzzleOutputState $ runLevelPuzzleWorld run 
        alpha = ostateAlpha $ levelpuzzleOutputState $ runLevelPuzzleWorld run
        tweak = sceneTweak $ runScene run
    --shadeSpaceColor sh tweak 1.0 modv $ smoothColor (colormapAt colormap ix0) 
    --                                                (colormapAt colormap ix1) alpha
    shadeSpace sh tweak 1.0 modv

    -- cube
    let sh = rundataShadeCube $ gamedataRunData gamedata
    shadeCube sh 1.0 projmodv modv 0.0 run

    -- message path
    let sh = griddataShadePath $ gamedataGridData gamedata
    shadePathBegin sh 1.0 projmodv
    shadePathRadius sh valueRunPathRadius
    shadePathColor sh valueRunPathColor
    shadePathDrawPath0 sh (gridPath $ runGrid run)
    shadePathEnd sh


--------------------------------------------------------------------------------
--  
    
drawFaceNames :: GameData -> Float -> Mat4 -> IO ()
drawFaceNames gamedata alpha projmodv = do
    let fsh = gamedataFontShade gamedata        
        ffd = gamedataFontData gamedata
        r = valueRunCubeRadius
    fontShade fsh alpha projmodv         
    fontDrawDefault fsh ffd valueRunCubeFontSize valueRunCubeFontColor
    fontSetPlane3D fsh 0 0 1 0 1 0
    fontDraw3DCentered fsh ffd (-r) 0 0 "LevelMode"
    fontSetPlane3D fsh 1 0 0 0 1 0
    fontDraw3DCentered fsh ffd 0 0 (r)  "GameCenter"
    fontSetPlane3D fsh 0 0 (-1) 0 1 0
    fontDraw3DCentered fsh ffd (r) 0 0  "PuzzleMode"
    fontSetPlane3D fsh (-1) 0 0 0 1 0
    fontDraw3DCentered fsh ffd 0 0 (-r) "MemoryMode"



drawFaceNamesBegin :: GameData -> Mat4 -> RunWorld -> IO ()
drawFaceNamesBegin gamedata projmodv run = do
    let fsh = gamedataFontShade gamedata        
        ffd = gamedataFontData gamedata
        r = valueRunCubeRadius
        fadeNames = fade fadeNamesTicksInv 0.0 (worldTick run)

    fontShade fsh fadeNames projmodv         
    fontDrawDefault fsh ffd valueRunCubeFontSize valueRunCubeFontColor
    fontSetPlane3D fsh 0 0 1 0 1 0
    fontDraw3DCentered fsh ffd (-r) 0 0 "LevelMode"
    fontSetPlane3D fsh 0 0 (-1) 0 1 0
    fontDraw3DCentered fsh ffd (r) 0 0  "PuzzleMode"
    fontSetPlane3D fsh (-1) 0 0 0 1 0
    fontDraw3DCentered fsh ffd 0 0 (-r) "MemoryMode"

    where
      fadeNamesTicks = 10.0
      fadeNamesTicksInv = 1.0 / fadeNamesTicks
      

{-
drawAtFaceCorners :: GameData -> Float -> Mat4 -> AtFaceState -> RunWorld -> IO ()
drawAtFaceCorners gamedata alpha projmodv s run = do
    let Shape wth hth = sceneShape $ runScene run
        wth' = valueRunCubeRadius * wth
        hth' = valueRunCubeRadius * hth
        r = valueRunCubeRadius * 1.001
    case atfacestateFace s of

        FaceLevelMode   -> 
            cornersDraw3D  gamedata alpha projmodv
                           (-r) (-hth') (-wth')
                           (hth * valueSceneCornerSize) 0.0 0.0 (2.0 * wth')
                           (wth * valueSceneCornerSize) 0.0 (2.0 * hth') 0.0

        FacePuzzleMode -> 
            cornersDraw3D  gamedata alpha projmodv
                           (r) (-hth') (wth')
                           (hth * valueSceneCornerSize) 0.0 0.0 (-2.0 * wth')
                           (wth * valueSceneCornerSize) 0.0 (2.0 * hth') 0.0
        
        FaceMemoryMode -> 
            cornersDraw3D  gamedata alpha projmodv
                           (wth') (-hth') (-r)
                           (hth * valueSceneCornerSize) ((-2.0) * wth') 0.0 0.0
                           (wth * valueSceneCornerSize) 0.0 (2.0 * hth') 0.0

        _              -> 
            return ()
      
    where
      cornersDraw3D :: GameData -> Float -> Mat4 -> 
                       Float -> Float -> Float -> 
                       Float -> Float -> Float -> Float -> 
                       Float -> Float -> Float -> Float -> IO ()
      cornersDraw3D gamedata alpha projmodv 
                    p0 p1 p2 
                    sizeX x0 x1 x2 
                    sizeY y0 y1 y2 =  do
          let sh = griddataShadeGeneral $ gamedataGridData gamedata
              cornerdata = rundataCornerData $ gamedataRunData gamedata
          shadeGeneral sh 1.0 projmodv
         
          glActiveTexture gl_TEXTURE0
          glBindTexture gl_TEXTURE_2D $ cornerdataTex cornerdata

          -- populate pos
          cornerdataWrite3D cornerdata p0 p1 p2 sizeX x0 x1 x2 sizeY y0 y1 y2

          -- draw
          glBindVertexArrayOES $ cornerdataVAO3D cornerdata
          glDrawArrays gl_TRIANGLE_STRIP 0 24

-}
--------------------------------------------------------------------------------
--  helpers

fade :: Float -> Tick -> Tick -> Float
fade scale t0 t1 =
    min 1.0 $ scale * rTF (t1 - t0)



