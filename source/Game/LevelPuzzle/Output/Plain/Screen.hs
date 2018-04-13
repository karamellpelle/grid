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
module Game.LevelPuzzle.Output.Plain.Screen
  (
    outputScreenBeginPlay,
    outputScreenPlay,
    outputScreenPlay',
    outputScreenComplete,
    outputScreenComplete',
    outputScreenSpecialComplete,
    outputScreenSpecialComplete',
    outputScreenFailure,
    outputScreenFailure',

  ) where

import MyPrelude
import Game

import Game.Font
import Game.Grid
import Game.Grid.Output
import Game.LevelPuzzle
import Game.LevelPuzzle.Iteration.State
import Game.LevelPuzzle.LevelPuzzleWorld.OutputState
import Game.LevelPuzzle.Output.Plain.ShadeWall
import Game.LevelPuzzle.Output.Plain.ShadeDot
import Game.Run.RunWorld
import Game.Run.RunData
import Game.Run.Output.Plain.ShadeCube

import OpenGL
import OpenGL.Helpers




--------------------------------------------------------------------------------
--  BeginPlay

outputScreenBeginPlay :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenBeginPlay gamedata lvl run = do
    return ()



--------------------------------------------------------------------------------
--  Play

outputScreenPlay :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenPlay gamedata lvl run = do
    return ()


outputScreenPlay' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                     s -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenPlay' gamedata proj modv normal s lvl run = do

    -- 3D --
    let projmodv = proj `mappend` modv
    glEnable gl_CULL_FACE
    glEnable gl_DEPTH_TEST
    
    let cnt0 = levelpuzzleOldContent lvl
        cnt1 = levelpuzzleContent lvl
        fadeContent = fade valueFadeContentTicksInv 0.0 (worldTick lvl)
        fadeRoom = fade valueFadeRoomTicksInv (contentEatTick cnt1) (worldTick lvl)
        color0 = gridColor gamedata $ contentRoom cnt0
        color1 = smoothColor (gridColor gamedata $ contentEatRoom cnt1)
                             (gridColor gamedata $ contentRoom cnt1)
                             fadeRoom
        color = smoothColor color0 color1 fadeContent

    -- SpaceBox
    let projmodv' = projmodv `mappend` mat4Turn (levelpuzzleRefSpace lvl)
    shadeSpaceBoxColor (griddataShadeSpaceBox $ gamedataGridData gamedata)
                       1.0 projmodv' proj valuePerspectiveRadius color

    -- OldContent?
    unless (1.0 <= fadeContent) $ do
        let projmodv' = projmodvRef projmodv $ levelpuzzleRefOldContent lvl
        drawContentRoom gamedata (1.0 - fadeContent) projmodv' projmodv' 
                        cnt0 (contentEatRoom cnt0)
        

    -- Content
    -- EatRoom?
    unless (1.0 <= fadeRoom) $ do
        drawContentRoom gamedata (1.0 - fadeRoom) projmodv normal 
                        cnt1 (contentEatRoom cnt1)

    -- Room
    drawContentRoom gamedata 1.0 projmodv normal 
                    cnt1 (contentRoom cnt1)

    -- Path
    shadePathBegin (griddataShadePath $ gamedataGridData gamedata)
                   1.0 projmodv
    shadePathColor (griddataShadePath $ gamedataGridData gamedata)
                   (Color 0.0 0.0 1.0 1.0)
    shadePathRadius (griddataShadePath $ gamedataGridData gamedata)
                    valueLevelPuzzlePathRadius

    drawContentPathRoom gamedata cnt1 (contentRoom cnt1)
    drawContentPathFront gamedata cnt1
    shadePathEnd (griddataShadePath $ gamedataGridData gamedata)

        

    -- 2D --
    let Shape wth hth = sceneShape $ runScene run
        projmodv = mat4Ortho2D 0 wth hth 0
    glDisable gl_CULL_FACE

    -- TextLevel
    drawTextLevel gamedata projmodv lvl run

    -- TextCount
    drawTextCount gamedata projmodv lvl run


    


--------------------------------------------------------------------------------
--  Failure

outputScreenFailure :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenFailure gamedata lvl run = do
    return ()


outputScreenFailure' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                        FailureS -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenFailure' gamedata proj modv normal s lvl run = do
    
    -- 3D --
    let projmodv = proj `mappend` modv
    glEnable gl_CULL_FACE
    glEnable gl_DEPTH_TEST
    
    let cnt0 = levelpuzzleOldContent lvl
        cnt1 = levelpuzzleContent lvl
        fadeContent = fade valueFadeContentTicksInv 0.0 (worldTick lvl)
        fadeRoom = fade valueFadeRoomTicksInv (contentEatTick cnt1) (worldTick lvl)
        fadeFailure = fade valueFadeFailureTicksInv (failuresTick s) (worldTick lvl) 
        color0 = gridColor gamedata $ contentRoom cnt0
        color1 = smoothColor (gridColor gamedata $ contentEatRoom cnt1)
                             (gridColor gamedata $ contentRoom cnt1)
                             fadeRoom
        color = smoothColor color0 color1 fadeContent

    -- SpaceBox
    let projmodv' = projmodv `mappend` mat4Turn (levelpuzzleRefSpace lvl)
    shadeSpaceBoxColor (griddataShadeSpaceBox $ gamedataGridData gamedata)
                       1.0 projmodv' proj valuePerspectiveRadius $ 
                       smoothColor color (Color 0.0 0.0 0.0 0.0) fadeFailure

    -- OldContent?
    unless (1.0 <= fadeContent) $ do
        let projmodv' = projmodvRef projmodv $ levelpuzzleRefOldContent lvl
        drawContentRoom gamedata (1.0 - fadeContent) projmodv' projmodv' 
                        cnt0 (contentEatRoom cnt0)
        

    -- Content
    -- EatRoom?
    unless (1.0 <= fadeRoom) $ do
        drawContentRoom gamedata (1.0 - fadeRoom) projmodv normal 
                        cnt1 (contentEatRoom cnt1)

    -- Room
    drawContentRoom gamedata 1.0 projmodv normal 
                    cnt1 (contentRoom cnt1)

    -- Path
    shadePathBegin (griddataShadePath $ gamedataGridData gamedata)
                   1.0 projmodv
    shadePathColor (griddataShadePath $ gamedataGridData gamedata)
                   (Color 0.0 0.0 1.0 1.0)
    shadePathRadius (griddataShadePath $ gamedataGridData gamedata)
                    valueLevelPuzzlePathRadius 

    drawContentPathRoom gamedata cnt1 (contentRoom cnt1)
    drawContentPathFront gamedata cnt1
    shadePathEnd (griddataShadePath $ gamedataGridData gamedata)


    -- 2D --
    let Shape wth hth = sceneShape $ runScene run
        projmodv = mat4Ortho2D 0 wth hth 0
    glDisable gl_CULL_FACE

    -- TextCount
    drawTextCount gamedata projmodv lvl run

    -- TextFailure
    drawTextFailure gamedata projmodv lvl run




--------------------------------------------------------------------------------
--  Complete

outputScreenComplete :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenComplete gamedata lvl run = do
    return ()

outputScreenComplete' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                         s -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenComplete' gamedata proj modv normal s lvl run = do
    
    -- 3D --
    let projmodv = proj `mappend` modv
    glEnable gl_CULL_FACE
    glEnable gl_DEPTH_TEST
    
    let cnt0 = levelpuzzleOldContent lvl
        cnt1 = levelpuzzleContent lvl
        fadeContent = fade valueFadeContentTicksInv 0.0 (worldTick lvl)
        fadeRoom = fade valueFadeRoomTicksInv (contentEatTick cnt1) (worldTick lvl)
        ostate = levelpuzzleOutputState lvl
        color0 = gridColor gamedata $ outputstateCompleteColorIx ostate
        color1 = gridColor gamedata $ outputstateCompleteColorIx' ostate
        color = smoothColor color0 color1 $ outputstateCompleteAlpha ostate
                       
    -- SpaceBox
    let projmodv' = projmodv `mappend` mat4Turn (levelpuzzleRefSpace lvl)
    shadeSpaceBoxColor (griddataShadeSpaceBox $ gamedataGridData gamedata)
                       1.0 projmodv' proj valuePerspectiveRadius color

    -- OldContent?
    unless (1.0 <= fadeContent) $ do
        let projmodv' = projmodvRef projmodv $ levelpuzzleRefOldContent lvl
        drawContentRoom gamedata (1.0 - fadeContent) projmodv' projmodv' 
                        cnt0 (contentEatRoom cnt0)

        shadePathBegin (griddataShadePath $ gamedataGridData gamedata)
                       1.0 projmodv
        shadePathColor (griddataShadePath $ gamedataGridData gamedata)
                       (Color 0.0 0.0 1.0 1.0)
        shadePathRadius (griddataShadePath $ gamedataGridData gamedata)
                        valueLevelPuzzlePathRadius

        drawContentPathRoom gamedata cnt0 (contentRoom cnt0)
        drawContentPathFront gamedata cnt0
        shadePathEnd (griddataShadePath $ gamedataGridData gamedata)
            
    
    -- 2D --
    let Shape wth hth = sceneShape $ runScene run
        projmodv = mat4Ortho2D 0 wth hth 0
    glDisable gl_CULL_FACE

    -- TextComplete
    drawTextComplete gamedata projmodv lvl run



--------------------------------------------------------------------------------
--  SpecialComplete

outputScreenSpecialComplete :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenSpecialComplete gamedata lvl run = do
    return ()


outputScreenSpecialComplete' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                                s -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenSpecialComplete' gamedata proj modv normal s lvl run = do

    -- 3D --
    let projmodv = proj `mappend` modv
    glEnable gl_CULL_FACE
    glEnable gl_DEPTH_TEST
    
    let cnt0 = levelpuzzleOldContent lvl
        cnt1 = levelpuzzleContent lvl
        fadeContent = fade valueFadeContentTicksInv 0.0 (worldTick lvl)
        fadeRoom = fade valueFadeRoomTicksInv (contentEatTick cnt1) (worldTick lvl)
        ostate = levelpuzzleOutputState lvl
        color0 = gridColor gamedata $ outputstateCompleteColorIx ostate
        color1 = gridColor gamedata $ outputstateCompleteColorIx' ostate
        color = smoothColor color0 color1 $ outputstateCompleteAlpha ostate
                       
    -- SpaceBox
    let projmodv' = projmodv `mappend` mat4Turn (levelpuzzleRefSpace lvl)
    shadeSpaceBoxColor (griddataShadeSpaceBox $ gamedataGridData gamedata)
                       1.0 projmodv' proj valuePerspectiveRadius color

    -- OldContent?
    unless (1.0 <= fadeContent) $ do
        let projmodv' = projmodvRef projmodv $ levelpuzzleRefOldContent lvl
        drawContentRoom gamedata (1.0 - fadeContent) projmodv' projmodv' 
                        cnt0 (contentEatRoom cnt0)

        shadePathBegin (griddataShadePath $ gamedataGridData gamedata)
                       1.0 projmodv
        shadePathColor (griddataShadePath $ gamedataGridData gamedata)
                       (Color 0.0 0.0 1.0 1.0)
        shadePathRadius (griddataShadePath $ gamedataGridData gamedata)
                        valueLevelPuzzlePathRadius

        drawContentPathRoom gamedata cnt0 (contentRoom cnt0)
        drawContentPathFront gamedata cnt0
        shadePathEnd (griddataShadePath $ gamedataGridData gamedata)
            
    
    -- RunCube
    let fadeRun = fade valueRunTicksInv 0.0 (worldTick lvl)
    shadeCube (rundataShadeCube $ gamedataRunData gamedata)
              fadeRun projmodv normal run

    -- Run Path
    shadePathFatBegin (griddataShadePath $ gamedataGridData gamedata)
                      fadeRun projmodv
    shadePathRadius (griddataShadePath $ gamedataGridData gamedata)
                    valueRunPathRadius
    shadePathColor (griddataShadePath $ gamedataGridData gamedata)
                   valueRunPathColor
    shadePathDrawPath0 (griddataShadePath $ gamedataGridData gamedata)
                       (gridPath $ runGrid run)
    shadePathEnd (griddataShadePath $ gamedataGridData gamedata)
    
    -- 2D --
    let Shape wth hth = sceneShape $ runScene run
        projmodv = mat4Ortho2D 0 wth hth 0
    glDisable gl_CULL_FACE

    -- TextSpecialComplete
    drawTextSpecialComplete gamedata projmodv lvl run


    where
      valueRunTicks = 16.0
      valueRunTicksInv = 1.0 / valueRunTicks



--------------------------------------------------------------------------------
--  

-- | draw Room of Content
drawContentRoom :: GameData -> Float -> Mat4 -> Mat4 -> Content -> RoomIx -> IO ()
drawContentRoom gamedata alpha projmodv normal cnt ix = do

    room <- roomarrayAt (contentRooms cnt) ix

    -- shadeDot --
    let sh = levelpuzzledataShadeDot $ gamedataLevelPuzzleData gamedata
    shadeDot sh alpha projmodv normal
    
    -- DotPlain
    shadeDotUseTexPlain sh
    dotplainarrayForIO_ (roomDotPlainSize room) (roomDotPlain room) $ \dot -> 
        unless (dotplainCount dot == 0) $ do
            shadeDotColor sh (gridColor gamedata $ dotplainRoom dot)
            shadeDotNode sh $ dotplainNode dot

    -- DotBonus
    shadeDotColor sh valueLevelPuzzleDotBonusColor
    shadeDotUseTexBonus sh
    dotbonusarrayForIO_ (roomDotBonusSize room) (roomDotBonus room) $ \dot -> 
        unless (dotbonusCount dot == 0) $ do
            shadeDotNode sh $ dotbonusNode dot

    -- DotTele
    shadeDotColor sh valueLevelPuzzleDotTeleColor
    dottelearrayForIO_ (roomDotTeleSize room) (roomDotTele room) $ \dot -> 
        unless (dotteleCount dot == 0) $ do
            shadeDotUseTexTele0 sh
            shadeDotNode sh $ dotteleNode dot
            shadeDotUseTexTele1 sh
            shadeDotNode sh $ dotteleNode' dot

    -- DotFinish
    shadeDotColor sh valueLevelPuzzleDotFinishColor
    shadeDotUseTexFinish sh
    dotfinisharrayForIO_ (roomDotFinishSize room) (roomDotFinish room) $ \dot -> 
        shadeDotNode sh $ dotfinishNode dot

{-
    -- shadeWall --
    let a = offsetarrayAt (contentdataWallOffsets $ contentData cnt) ix
        b = offsetarrayAt (contentdataWallOffsets $ contentData cnt) (ix + 1)
    shadeWall (levelpuzzledataShadeWall $ gamedataLevelPuzzleData gamedata)
              alpha projmodv normal
    glBindVertexArrayOES $ contentdataWallVAO $ contentData cnt
    glDrawElements gl_TRIANGLE_STRIP (fI $ b - a) gl_UNSIGNED_SHORT (mkPtrGLvoid $ 2 * a)
-}

-- | draw Path of Room in Content
drawContentPathRoom :: GameData -> Content -> RoomIx -> IO ()
drawContentPathRoom gamedata cnt ix = do
    room <- roomarrayAt (contentRooms cnt) ix
    forM_ (roomPathBeginEnd room) $ \(begin, end) -> 
        shadePathDrawPath0BeginEnd (griddataShadePath $ gamedataGridData gamedata) 
                                   begin end (gridPath $ contentGrid cnt)


-- | draw front of Path in Content, which typically is in current Room
drawContentPathFront :: GameData -> Content -> IO ()
drawContentPathFront gamedata cnt = do
    shadePathDrawBegin (griddataShadePath $ gamedataGridData gamedata)
                       (contentEatPathBegin cnt) (gridPath $ contentGrid cnt)




-- | draw info about current Level
drawTextLevel :: GameData -> Mat4 -> LevelPuzzleWorld -> RunWorld -> IO ()
drawTextLevel gamedata projmodv lvl run = do
    let fadeText = fade valueLevelTicksInv 0.0 (worldTick lvl)
    unless (1.0 <= fadeText) $ do
        let fsh = gamedataFontShade gamedata        
            ffd = gamedataFontData gamedata
            alpha = 1.0 - fadeText
            Shape wth hth = sceneShape $ runScene run
        fontShade fsh alpha projmodv         
        fontDrawDefault fsh ffd (valueTextFontBSize * hth) valueTextFontBColor
        fontDraw2DCentered fsh ffd (0.5 * wth) (valueTextFontBY * hth) $ 
                           "(level " ++ show (levelpuzzleLevelIx lvl) ++ ")"
        fontDrawDefault fsh ffd (valueTextFontASize * hth) valueTextFontAColor 
        fontDraw2DCentered fsh ffd (0.5 * wth) (valueTextFontAY * hth) $ 
                           levelName $ levelpuzzleLevel lvl

    where
      valueLevelTicks     = 8.0
      valueLevelTicksInv  = 1.0 / valueLevelTicks


-- | draw current Count
drawTextCount :: GameData -> Mat4 -> LevelPuzzleWorld -> RunWorld -> IO ()
drawTextCount gamedata projmodv lvl run = do
    let fsh = gamedataFontShade gamedata        
        ffd = gamedataFontData gamedata
        Shape wth hth = sceneShape $ runScene run
    fontShade fsh 1.0 projmodv         
    fontDrawDefault fsh ffd (valueTextFontCSize * hth) valueTextFontCColor 
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueTextFontCY * hth) $  
                       "count: " ++ show (levelpuzzleSegmentsCount lvl)


-- | draw Failure message
drawTextFailure :: GameData -> Mat4 -> LevelPuzzleWorld -> RunWorld -> IO ()
drawTextFailure  gamedata projmodv lvl run = do
    let fsh = gamedataFontShade gamedata        
        ffd = gamedataFontData gamedata
        Shape wth hth = sceneShape $ runScene run
    fontShade fsh 1.0 projmodv         
    fontDrawDefault fsh ffd (valueTextFontASize * hth) valueTextFontAColor 
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueTextFontAY * hth) $ 
        case levelpuzzleFailureEvent lvl of
            EventPathEatWall  -> "wall hit :("
            EventNullSegments -> "count empty :("
            _                 -> ""


-- | draw text when Complete
drawTextComplete :: GameData -> Mat4 -> LevelPuzzleWorld -> RunWorld -> IO ()
drawTextComplete gamedata projmodv lvl run = do
    let fsh = gamedataFontShade gamedata        
        ffd = gamedataFontData gamedata
        Shape wth hth = sceneShape $ runScene run
        fadeText = min valueCompleteTrans $ 
                   fade valueCompleteTicksInv 0.0 (worldTick lvl)
    fontShade fsh (1.0 - fadeText) projmodv         
    fontDrawDefault fsh ffd (valueCompleteFont0Size * hth) valueCompleteFont0Color
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueCompleteFont0Y0 * hth) $ 
                       levelpuzzleName lvl 
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueCompleteFont0Y1 * hth) $ 
                       "completed"
    
    fontDrawDefault fsh ffd (valueCompleteFont1Size * hth) valueCompleteFont1Color
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueCompleteFont1Y * hth) $ 
                       ":)"
    
    where
      valueCompleteFont0Size  = 0.1
      valueCompleteFont0Color = FontColor 0.0 1.0 0.0 1.0
      valueCompleteFont0Y0    = 0.1
      valueCompleteFont0Y1    = 0.2
      valueCompleteFont1Size  = 0.24
      valueCompleteFont1Color = FontColor 1.0 0.0 0.0 1.0
      valueCompleteFont1Y     = 0.3
      valueCompleteTicks      = 16.0
      valueCompleteTicksInv   = 1.0 / valueCompleteTicks 
      valueCompleteTrans      = 0.9


-- | draw text when SpecialComplete
drawTextSpecialComplete :: GameData -> Mat4 -> LevelPuzzleWorld -> RunWorld -> IO ()
drawTextSpecialComplete gamedata projmodv lvl run = do
    let fsh = gamedataFontShade gamedata        
        ffd = gamedataFontData gamedata
        Shape wth hth = sceneShape $ runScene run
        fadeText = min valueSpecialCompleteTrans $ 
                   fade valueSpecialCompleteTicksInv 0.0 (worldTick lvl)
    fontShade fsh (1.0 - fadeText) projmodv         
    fontDrawDefault fsh ffd (valueSpecialCompleteFont0Size * hth) valueSpecialCompleteFont0Color
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueSpecialCompleteFont0Y * hth) $ 
                       "complete"
    
    fontDrawDefault fsh ffd (valueSpecialCompleteFont1Size * hth) valueSpecialCompleteFont1Color
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueSpecialCompleteFont1Y * hth) $ 
                       ":)"
    
    where
      valueSpecialCompleteFont0Size  = 0.1
      valueSpecialCompleteFont0Color = FontColor 0.0 1.0 0.0 1.0
      valueSpecialCompleteFont0Y     = 0.1
      valueSpecialCompleteFont1Size  = 0.24
      valueSpecialCompleteFont1Color = FontColor 1.0 0.0 0.0 1.0
      valueSpecialCompleteFont1Y     = 0.3
      valueSpecialCompleteTicks     = 16.0
      valueSpecialCompleteTicksInv  = 1.0 / valueSpecialCompleteTicks 
      valueSpecialCompleteTrans     = 0.9




--------------------------------------------------------------------------------
--  helpers

fade :: Float -> Tick -> Tick -> Float
fade scale t0 t1 = 
    min 1.0 $ rTF (t1 - t0) * scale


projmodvRef :: Mat4 -> Segment -> Mat4
projmodvRef projmodv (Segment node turn) =
    mat4TranslateNode (mat4RotateTurn projmodv turn) node 


gridColor :: GameData -> UInt -> Color
gridColor gamedata ix = 
    colormapAt (griddataColorMap $ gamedataGridData gamedata) ix
    

