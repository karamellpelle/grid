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
module Game.LevelPuzzleMode.Output.Fancy.Screen
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

import Game.Grid.GridData
import Game.Grid.Helpers
import Game.Grid.Output
import Game.Grid.Output.Fancy

import Game.LevelPuzzleMode.LevelPuzzleWorld
import Game.LevelPuzzleMode.LevelPuzzleData
import Game.LevelPuzzleMode.Helpers
import Game.LevelPuzzleMode.Iteration.State

import Game.Run.RunWorld

import OpenGL
import OpenGL.Helpers
import Game.Grid.Output.Fancy.DrawPath
import Game.LevelPuzzleMode.Output.Fancy.Draw

import Graphics.UI.GLFW

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
                     s -> LevelPuzzleWorld -> RunWorld -> MEnv' ()
outputScreenPlay' gamedata proj modv normal s lvl run = do
    (swth, shth) <- screenSize
    io $ glViewport 0 0 (fI swth) (fI shth)

    io $ do
        -- 3D --
        draw3D gamedata proj modv normal lvl run

        -- 2D --
        let proj = mat4Ortho2D 0 (fI swth) (- fI shth) 0
        glDisable gl_CULL_FACE
        glDisable gl_DEPTH_TEST
        setProjModV proj mempty
       
        glColor4f 1 1 1 1
        drawTextLevel gamedata lvl          

{-
    -- 2D --
    let Shape wth hth = sceneShape $ runScene run
        projmodv = mat4Ortho2D 0 wth hth 0
    glDisable gl_CULL_FACE

    -- segment count
    drawTextSegmentCount gamedata projmodv lvl

    -- Level
    drawTextLevel gamedata projmodv lvl
    
-}

--------------------------------------------------------------------------------
--  Failure

outputScreenFailure :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenFailure gamedata lvl run = do
    return ()


outputScreenFailure' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                        FailureS -> LevelPuzzleWorld -> RunWorld -> MEnv' ()
outputScreenFailure' gamedata proj modv normal s lvl run = do
    (swth, shth) <- screenSize
    io $ glViewport 0 0 (fI swth) (fI shth)

    io $ do
        -- 3D --
        draw3D gamedata proj modv normal lvl run

        -- 2D --
        let proj = mat4Ortho2D 0 (fI swth) (- fI shth) 0
        glDisable gl_CULL_FACE
        glDisable gl_DEPTH_TEST
        setProjModV proj mempty
       
        glColor4f 1 1 1 1
        drawTextLevel gamedata lvl
        drawTextFailure gamedata lvl


--------------------------------------------------------------------------------
--  Complete

outputScreenComplete :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenComplete gamedata lvl run = do
    return ()

outputScreenComplete' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                         s -> LevelPuzzleWorld -> RunWorld -> MEnv' ()
outputScreenComplete' gamedata proj modv normal s lvl run = do
    (swth, shth) <- screenSize
    io $ glViewport 0 0 (fI swth) (fI shth)
    io $ do
        -- 3D --
        draw3D gamedata proj modv normal lvl run
   
        -- 2D --
        let proj = mat4Ortho2D 0 (fI swth) (- fI shth) 0
        glDisable gl_CULL_FACE
        glDisable gl_DEPTH_TEST
        setProjModV proj mempty
       
        glColor4f 1 1 1 1
        drawTextLevel gamedata lvl
        drawTextComplete gamedata lvl



--------------------------------------------------------------------------------
--  SpecialComplete

outputScreenSpecialComplete :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputScreenSpecialComplete gamedata lvl run = do
    return ()

outputScreenSpecialComplete' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                                s -> LevelPuzzleWorld -> RunWorld -> MEnv' ()
outputScreenSpecialComplete' = 
    outputScreenComplete'

--------------------------------------------------------------------------------
---  


draw3D gamedata proj modv normal lvl run = do

    -- 3D --
    glEnable gl_DEPTH_TEST
    glEnable gl_CULL_FACE
    setProjModV proj modv

    let colormap = griddataColorMap $ gamedataGridData gamedata
        room = contentRoom $ levelpuzzleContent lvl

    -- space box
    drawSpaceBoxColor $ colormapAt colormap room
   
    -- current room
    let room' = contentRoom $ levelpuzzleContent lvl
    drawContentRoom gamedata 1.0 (levelpuzzleContent lvl) room'
    
    
    -- path of current room
    drawPathBegin
    drawPathColor $ Color 0 0 1 1 
    drawContentPathRoom gamedata (levelpuzzleContent lvl) room'
    drawContentPathFront gamedata $ levelpuzzleContent lvl
    drawPathEnd



drawContentRoom :: GameData -> Float -> Content -> RoomIx -> IO ()
drawContentRoom gamedata alpha cnt ix = unless (contentRoomsSize cnt == 0) $ do
    let alpha' = rTF alpha
    room <- roomarrayAt (contentRooms cnt) ix

    let colormap = griddataColorMap $ gamedataGridData gamedata

    -- dotplain
    dotplainarrayForIO_ (roomDotPlainSize room) (roomDotPlain room) $ \dot -> 
        unless (dotplainCount dot == 0) $
            drawDotPlain alpha' gamedata dot

    -- dotbonus
    dotbonusarrayForIO_ (roomDotBonusSize room) (roomDotBonus room) $ \dot -> 
        unless (dotbonusCount dot == 0) $ do
            drawDotBonus alpha' gamedata dot

    -- dottele
    dottelearrayForIO_ (roomDotTeleSize room) (roomDotTele room) $ \dot -> 
        unless (dotteleCount dot == 0) $ do
            drawDotTele alpha' gamedata dot

    -- dotfinish
    dotfinisharrayForIO_ (roomDotFinishSize room) (roomDotFinish room) $ \dot -> 
        drawDotFinish alpha' gamedata dot

    
    wallarrayForIO_ (roomWallSize room) (roomWall room) $ \wall ->
        drawWall alpha' gamedata wall


--------------------------------------------------------------------------------
--  draw Path

drawContentPathRoom :: GameData -> Content -> RoomIx -> IO ()
drawContentPathRoom gamedata cnt ix = unless (contentRoomsSize cnt == 0) $ do
    room <- roomarrayAt (contentRooms cnt) ix
    forM_ (roomPathBeginEnd room) $ \(begin, end) -> 
        drawPathDrawPath0BeginEnd begin end (gridPathA $ contentGrid cnt)

drawContentPathFront :: GameData -> Content -> IO ()
drawContentPathFront gamedata cnt = do
    drawPathDrawBegin (contentEatPathBegin cnt) (gridPathA $ contentGrid cnt)


--------------------------------------------------------------------------------
--  

projmodvRef :: Mat4 -> Segment -> Mat4
projmodvRef projmodv (Segment node turn) =
    mat4TranslateNode (mat4RotateTurn projmodv turn) node 



--------------------------------------------------------------------------------
--  

--------------------------------------------------------------------------------
--  text


drawStrings :: Int -> [String] -> IO ()
drawStrings line strs = do
    --foldM (\y str -> drawString 0 y str >> return (y + 20)) 20 strs
    foldM (\y str -> (drawString 0 y str >> return (y - 20))) ((line + 1) * (-20)) strs
    return ()

drawString :: Int -> Int -> String -> IO ()
drawString x y str = do
    glPushMatrix
    glTranslatef (fI x) (fI y) 0.0
    renderString Fixed8x16 str
    glPopMatrix 


drawTextLevel :: GameData -> LevelPuzzleWorld -> IO ()
drawTextLevel gamedata lvl = do
    drawStrings 0 $ [ "LevelName:          " ++ levelName (levelpuzzleLevel lvl),
                      "SegmentsCount:       " ++ show (levelpuzzleSegmentsCount lvl) ]

drawTextFailure ::GameData -> LevelPuzzleWorld -> IO () 
drawTextFailure  gamedata lvl = do
    drawStrings 2 $ ["Failure              " ++  case levelpuzzleFailureEvent lvl of
                            EventPathEatWall  -> "wall hit :("
                            EventNullSegments -> "segments empty :(",
                     "(press Enter for restart)" ]

drawTextComplete :: GameData -> LevelPuzzleWorld -> IO ()
drawTextComplete gamedata lvl = do
    drawStrings 2 $ ["Complete:            " ++ ":)" ]


