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
module Game.LevelPuzzle.Output.Fancy.Screen
  (
    outputScreenBeginPlay,
    outputScreenPlay,
    outputScreenPlay',
    outputScreenComplete,
    outputScreenComplete',
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
import Game.LevelPuzzle.Output.Fancy.ShadeWall
import Game.LevelPuzzle.Output.Fancy.ShadeDot
import Game.Run.RunWorld
import Game.Run.RunData
import Game.Run.Output.Fancy.ShadeCube

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
outputScreenPlay' gamedata proj2D proj3D modv3D = \s lvl run -> do

    -- 3D --
    let modv = modv3D
        projmodv = proj3D `mappend` modv
        normal = modv
        tmod = cameraTurnModelMat4 $ levelpuzzleCamera lvl
    
    let sh = griddataShadeSpace $ gamedataGridData gamedata
        cnt0 = levelpuzzleOldContent lvl
        cnt1 = levelpuzzleContent lvl
        fadeContent = fade valueFadeContentTicksInv 0.0 (worldTick lvl)
        fadeRoom = fade valueFadeRoomTicksInv (contentEatTick cnt1) (worldTick lvl)
        color0 = gridColor gamedata $ contentRoom cnt0
        color1 = smoothColor (gridColor gamedata $ contentEatRoom cnt1)
                             (gridColor gamedata $ contentRoom cnt1)
                             fadeRoom
        color = smoothColor color0 color1 fadeContent
        tweak = sceneTweak $ runScene run

    -- Space
    let refmodv = refCurrentMat4 lvl modv
    shadeSpaceColor sh tweak 1.0 refmodv color
    
    -- OldContent?
    unless (1.0 <= fadeContent) $ do
        let refprojmodv = refOldMat4 lvl projmodv
            refnormal = refOldMat4 lvl normal
            reftmod = refOldMat4 lvl tmod
        drawContentRoom gamedata (1.0 - fadeContent) refprojmodv refnormal reftmod
                        cnt0 (contentEatRoom cnt0)

    -- Content
    -- EatRoom?
    unless (1.0 <= fadeRoom) $ do
        drawContentRoom gamedata (1.0 - fadeRoom) projmodv normal tmod 
                        cnt1 (contentEatRoom cnt1)

    -- Room
    drawContentRoom gamedata 1.0 projmodv normal tmod cnt1 (contentRoom cnt1)

    -- Path
    let sh = griddataShadePath $ gamedataGridData gamedata
    shadePathBegin sh 1.0 projmodv
    shadePathColor sh (Color 0.0 0.0 1.0 1.0)
    shadePathRadius sh valueLevelPuzzlePathRadius

    drawContentPathRoom gamedata cnt1 (contentRoom cnt1)
    drawContentPathFront gamedata cnt1
    shadePathEnd sh

    -- Bonus
    drawBonus gamedata projmodv lvl run 

    -- 2D --
    let projmodv = proj2D
    glDisable gl_CULL_FACE

    -- TextLevel
    drawTextLevel gamedata projmodv lvl run

    -- TextCount
    drawTextCount gamedata 1.0 projmodv lvl run

    -- TextBonus
    --drawTextBonus gamedata projmodv lvl run
   
    glEnable gl_CULL_FACE


--------------------------------------------------------------------------------
--  Failure

outputScreenFailure :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenFailure gamedata lvl run = do
    return ()


outputScreenFailure' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                        FailureS -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenFailure' gamedata proj2D proj3D modv3D = \s lvl run -> do
    
    -- 3D --
    let modv = modv3D
        projmodv = proj3D `mappend` modv
        normal = modv3D
        tmod = cameraTurnModelMat4 $ levelpuzzleCamera lvl

    let fadeSpace = 1.0 - fade valueFadeSpaceInv (failuresTick s) (worldTick lvl) 
        fadeCount = 1.0 - fade valueFadeCountInv (failuresTick s) (worldTick lvl) 

    let sh = griddataShadeSpace $ gamedataGridData gamedata
        cnt0 = levelpuzzleOldContent lvl
        cnt1 = levelpuzzleContent lvl
        fadeContent = fade valueFadeContentTicksInv 0.0 (worldTick lvl)
        fadeRoom = fade valueFadeRoomTicksInv (contentEatTick cnt1) (worldTick lvl)
        color0 = gridColor gamedata $ contentRoom cnt0
        color1 = smoothColor (gridColor gamedata $ contentEatRoom cnt1)
                             (gridColor gamedata $ contentRoom cnt1)
                             fadeRoom
        color = smoothColor color0 color1 fadeContent
        tweak = sceneTweak $ runScene run

    -- Space
    let refmodv = refCurrentMat4 lvl modv 
    shadeSpaceColor sh tweak 1.0 refmodv $ smoothColor colorWhite color fadeSpace

    -- OldContent?
    unless (1.0 <= fadeContent) $ do
        let refprojmodv = refOldMat4 lvl projmodv 
            refnormal = refOldMat4 lvl normal
            reftmod = refOldMat4 lvl tmod
        drawContentRoom gamedata (1.0 - fadeContent) refprojmodv refnormal reftmod
                        cnt0 (contentEatRoom cnt0)
        

    -- Content
    -- EatRoom?
    unless (1.0 <= fadeRoom) $ do
        drawContentRoom gamedata (1.0 - fadeRoom) projmodv normal tmod 
                        cnt1 (contentEatRoom cnt1)

    -- Room
    drawContentRoom gamedata 1.0 projmodv normal tmod cnt1 (contentRoom cnt1)

    -- Path
    let sh = griddataShadePath $ gamedataGridData gamedata
    shadePathBegin sh 1.0 projmodv
    shadePathColor sh (Color 0.0 0.0 1.0 1.0)
    shadePathRadius sh valueLevelPuzzlePathRadius

    drawContentPathRoom gamedata cnt1 (contentRoom cnt1)
    drawContentPathFront gamedata cnt1
    shadePathEnd sh


    -- 2D --
    let projmodv = proj2D
    glDisable gl_CULL_FACE

    -- TextCount
    drawTextCount gamedata fadeCount projmodv lvl run

    -- TextFailure
    drawTextFailure gamedata projmodv s lvl run
    
    glEnable gl_CULL_FACE

    where
      valueFadeSpace = 1.0
      valueFadeSpaceInv = 1.0 / valueFadeSpace
      valueFadeCount = 1.0
      valueFadeCountInv = 1.0 / valueFadeCount




--------------------------------------------------------------------------------
--  Complete

outputScreenComplete :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenComplete gamedata lvl run = do
    return ()


outputScreenComplete' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                         CompleteS -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenComplete' gamedata proj2D proj3D modv3D = \s lvl run -> do

    -- 3D --
    let modv = modv3D
        projmodv = proj3D `mappend` modv
        normal = modv
        tmod = cameraTurnModelMat4 $ levelpuzzleCamera lvl
    
    let sh = griddataShadeSpace $ gamedataGridData gamedata
        cnt0 = levelpuzzleOldContent lvl
        cnt1 = levelpuzzleContent lvl
        fadeContent = fade valueFadeContentTicksInv 0.0 (worldTick lvl)
        fadeRoom = fade valueFadeRoomTicksInv (contentEatTick cnt1) (worldTick lvl)
        ostate = levelpuzzleOutputState lvl
        color0 = gridColor gamedata $ ostateColorIx0 ostate
        color1 = gridColor gamedata $ ostateColorIx1 ostate
        color = smoothColor color0 color1 $ ostateAlpha ostate
        tweak = sceneTweak $ runScene run

    -- Space
    let refmodv = refCurrentMat4 lvl modv
    shadeSpaceColor sh tweak 1.0 refmodv color

    -- OldContent?
    let refprojmodv = refOldMat4 lvl projmodv 
        refnormal = refOldMat4 lvl normal
        reftmod = refOldMat4 lvl tmod
    drawContentRoom gamedata 1.0 refprojmodv refnormal reftmod cnt0 (contentEatRoom cnt0)

    -- Path --
    let sh = griddataShadePath $ gamedataGridData gamedata
    shadePathBegin sh 1.0 projmodv
    shadePathColor sh (Color 0.0 0.0 1.0 1.0)
    shadePathRadius sh valueLevelPuzzlePathRadius

    drawContentPathRoom gamedata cnt0 (contentRoom cnt0)
    drawContentPathFront gamedata cnt0
    shadePathEnd sh
          
  
    -- RunCube
    let sh = rundataShadeCube $ gamedataRunData gamedata
        fadeRun = 0.4 * fade valueRunTicksInv 0.0 (worldTick lvl)
    shadeCube sh fadeRun projmodv normal 0.0 run

    -- Run Path
    let sh = griddataShadePath $ gamedataGridData gamedata
    shadePathBegin sh fadeRun projmodv
    shadePathRadius sh valueRunPathRadius
    shadePathColor sh valueRunPathColor
    shadePathDrawPath0 sh (gridPath $ runGrid run)
    shadePathEnd sh
    
    -- 2D --
    let projmodv = proj2D
    glDisable gl_CULL_FACE

    -- TextComplete
    drawTextComplete gamedata projmodv lvl run

    glEnable gl_CULL_FACE

    where
      valueRunTicks = 16.0
      valueRunTicksInv = 1.0 / valueRunTicks



--------------------------------------------------------------------------------
--  

-- | draw Room of Content
drawContentRoom :: GameData -> Float -> Mat4 -> Mat4 -> Mat4 -> Content -> RoomIx -> IO ()
drawContentRoom gamedata alpha projmodv normal tmod cnt ix = do

    room <- roomarrayAt (contentRooms cnt) ix

    -- shadeDot --
    let sh = levelpuzzledataShadeDot $ gamedataLevelPuzzleData gamedata
    shadeDotBegin sh alpha projmodv tmod
    
    -- DotPlain
    shadeDotRadius sh valueDotPlainRadius
    shadeDotUseTexPlain sh
    dotplainarrayForIO_ (roomDotPlainSize room) (roomDotPlain room) $ \dot -> 
        unless (dotplainCount dot == 0) $ do
            shadeDotColor sh (gridColor gamedata $ dotplainRoom dot)
            shadeDotNode sh $ dotplainNode dot

    shadeDotColor sh colorNull

    -- DotBonus
    shadeDotRadius sh valueDotBonusRadius
    shadeDotUseTexBonus sh
    dotbonusarrayForIO_ (roomDotBonusSize room) (roomDotBonus room) $ \dot -> 
        unless (dotbonusCount dot == 0) $ do
            shadeDotNode sh $ dotbonusNode dot

    -- DotTele
    shadeDotRadius sh valueDotTeleRadius
    shadeDotUseTexTele0 sh
    dottelearrayForIO_ (roomDotTeleSize room) (roomDotTele room) $ \dot -> 
        unless (dotteleCount dot == 0) $ shadeDotNode sh $ dotteleNode dot
    shadeDotUseTexTele1 sh
    dottelearrayForIO_ (roomDotTeleSize room) (roomDotTele room) $ \dot -> 
        {-unless (dotteleCount dot == 0) $ -}shadeDotNode sh $ dotteleNode' dot

    -- DotFinish
    shadeDotRadius sh valueDotFinishRadius
    shadeDotUseTexFinish sh
    dotfinisharrayForIO_ (roomDotFinishSize room) (roomDotFinish room) $ \dot -> 
        shadeDotNode sh $ dotfinishNode dot

    shadeDotEnd sh


    -- shadeWall --
    let sh = levelpuzzledataShadeWall $ gamedataLevelPuzzleData gamedata
        a = offsetarrayAt (contentdataWallOffsets $ contentData cnt) ix
        b = offsetarrayAt (contentdataWallOffsets $ contentData cnt) (ix + 1)
    shadeWallBegin sh alpha projmodv normal
    glBindVertexArrayOES $ contentdataWallVAO $ contentData cnt
    glDrawArrays gl_TRIANGLE_STRIP (fI a) (fI $ b - a)
    shadeWallEnd sh

    where
      valueDotPlainRadius = 0.4
      valueDotBonusRadius = 0.4
      valueDotTeleRadius = 0.4
      valueDotFinishRadius = 0.4


-- | draw Path of Room in Content
drawContentPathRoom :: GameData -> Content -> RoomIx -> IO ()
drawContentPathRoom gamedata cnt ix = do
    room <- roomarrayAt (contentRooms cnt) ix
    forM_ (roomPathBeginEnd room) $ \(begin, end) -> do
        let sh = griddataShadePath $ gamedataGridData gamedata
            path = gridPath $ contentGrid cnt
        shadePathDrawPath0BeginEnd sh begin end path


-- | draw front of Path in Content, which typically is in current Room
drawContentPathFront :: GameData -> Content -> IO ()
drawContentPathFront gamedata cnt = do
    let sh = griddataShadePath $ gamedataGridData gamedata
    shadePathDrawBegin sh (contentEatPathBegin cnt) (gridPath $ contentGrid cnt)




-- | draw Bonus Add
drawBonus :: GameData -> Mat4 -> LevelPuzzleWorld -> RunWorld -> IO ()
drawBonus gamedata projmodv lvl run = do
    let ostate = levelpuzzleOutputState lvl
        fadeText = fade valueBonusTicksInv (ostateBonusTick ostate) (worldTick lvl)
    unless (1.0 <= fadeText || ostateBonusAdd ostate == 0) $ do
        let fsh = gamedataFontShade gamedata        
            ffd = gamedataFontData gamedata
            alpha = 1.0 - fadeText
            size = (1.0 + scaleSize * fadeText) * valueTextFontBonusSize
            Segment (Node x y z) (Turn a0 a1 a2 
                                       b0 b1 b2 
                                       c0 c1 c2) = ostateBonusRef ostate
        
        glDisable gl_CULL_FACE
        fontShade fsh alpha projmodv
        fontSetPlane3D fsh (fI c0) (fI c1) (fI c2) 
                           (fI b0) (fI b1) (fI b2)
        fontDrawDefault fsh ffd size $ mkFontColor $ ostateBonusColor ostate 
        fontDraw3DCentered fsh ffd (fI x) (fI y) (fI z) $ 
                           "+" ++ show (ostateBonusAdd ostate)
        glEnable gl_CULL_FACE

    where
      mkFontColor (Color r g b a) = 
          FontColor r g b a
      valueBonusTicks         = 2.0
      valueBonusTicksInv      = 1.0 / valueBonusTicks
      valueTextFontBonusSize  = 0.8
      scaleSize               = 5.0



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
drawTextCount :: GameData -> Float -> Mat4 -> LevelPuzzleWorld -> RunWorld -> IO ()
drawTextCount gamedata alpha projmodv lvl run = do
    let fsh = gamedataFontShade gamedata        
        ffd = gamedataFontData gamedata
        Shape wth hth = sceneShape $ runScene run
    fontShade fsh alpha projmodv         
    fontDrawDefault fsh ffd (valueTextFontCSize * hth) valueTextFontCColor 
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueTextFontCY * hth) $  
                       "count: " ++ show (levelpuzzleSegmentsCount lvl)


-- | draw Bonus Add
drawTextBonus :: GameData -> Mat4 -> LevelPuzzleWorld -> RunWorld -> IO ()
drawTextBonus gamedata projmodv lvl run = do
    let ostate = levelpuzzleOutputState lvl
        fadeText = fade valueBonusTicksInv (ostateBonusTick ostate) (worldTick lvl)
    unless (1.0 <= fadeText || ostateBonusAdd ostate == 0) $ do
        let fsh = gamedataFontShade gamedata        
            ffd = gamedataFontData gamedata
            alpha = 1.0 - fadeText
            Shape wth hth = sceneShape $ runScene run
            size = (1.0 + scaleSize * fadeText) * valueTextFontCSize * hth
            y = (valueTextFontCY + fadeText * translateY) * hth
        fontShade fsh alpha projmodv         
        fontDrawDefault fsh ffd size valueTextFontCColor 
        fontDraw2DCentered fsh ffd (0.5 * wth) y $  
                           "+" ++ show (ostateBonusAdd ostate)

    where
      valueBonusTicks     = 2.0
      valueBonusTicksInv  = 1.0 / valueBonusTicks
      translateY          = (-0.04)
      scaleSize           = 1.0



-- | draw Failure message
drawTextFailure :: GameData -> Mat4 -> FailureS -> LevelPuzzleWorld -> RunWorld -> IO ()
drawTextFailure  gamedata projmodv s lvl run = do
    let fsh = gamedataFontShade gamedata        
        ffd = gamedataFontData gamedata
        Shape wth hth = sceneShape $ runScene run
    fontShade fsh 1.0 projmodv         
    fontDrawDefault fsh ffd (valueTextFontBSize * hth) valueTextFontBColor
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueTextFontBY * hth) $ 
                        "tap to restart"
    fontDrawDefault fsh ffd (valueTextFontASize * hth) valueTextFontAColor 
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueTextFontAY * hth) $ 
        case levelpuzzleFailureEvent lvl of
            EventPathEatWall  wall  -> "wall hit :("
            EventNullSegments       -> "count empty :("
            _                       -> ""


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
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueCompleteFont0Y * hth) $ 
                       "complete"
    
    fontDrawDefault fsh ffd (valueCompleteFont1Size * hth) valueCompleteFont1Color
    fontDraw2DCentered fsh ffd (0.5 * wth) (valueCompleteFont1Y * hth) $ 
                       ":)"
    
    where
      valueCompleteFont0Size  = 0.28
      valueCompleteFont0Color = FontColor 0.0 1.0 0.0 1.0
      valueCompleteFont0Y     = 0.3
      valueCompleteFont1Size  = 0.3
      valueCompleteFont1Color = FontColor 1.0 0.0 1.0 1.0
      valueCompleteFont1Y     = 0.5
      valueCompleteTicks     = 16.0
      valueCompleteTicksInv  = 1.0 / valueCompleteTicks 
      valueCompleteTrans     = 0.5




--------------------------------------------------------------------------------
--  helpers

fade :: Float -> Tick -> Tick -> Float
fade scale t0 t1 = 
    min 1.0 $ rTF (t1 - t0) * scale


-- | transform Mat4 for current Content (relative to RefSpace)
refCurrentMat4 :: LevelPuzzleWorld -> Mat4 -> Mat4
refCurrentMat4 lvl mat =
    mat `mappend` mat4Turn (levelpuzzleRefSpace lvl)


-- | transform Mat4 for old Content (relative to RefOldContent)
refOldMat4 :: LevelPuzzleWorld -> Mat4 -> Mat4
refOldMat4 lvl mat =
    case levelpuzzleRefOldContent lvl of
        Segment node turn -> mat4TranslateNode (mat4RotateTurn mat turn) node 


cameraTurnModelMat4 :: Camera -> Mat4
cameraTurnModelMat4 cam = 
    (Mat4 0.0 0.0 (-1.0) 0.0
          0.0 1.0 0.0 0.0
          1.0 0.0 0.0 0.0
          0.0 0.0 0.0 1.0) `mappend` cameraModelMat4 cam

gridColor :: GameData -> UInt -> Color
gridColor gamedata ix = 
    colormapAt2Safe (griddataColorMap $ gamedataGridData gamedata) ix
    


