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
module Test.Tweak.Output
  (
    outputTestTweak',
  
  ) where


import MyPrelude
import Game

import Text.Printf
import Game.Grid.Helpers
import Game.Grid.Output
import Game.Grid.Output.Fancy
import Game.Run.RunWorld
import Game.Run.RunData
import Game.Run.Iteration.State
import Game.Run.Output.Fancy.Sound
import Game.Run.Output.Fancy.Screen
import OpenGL
import OpenGL.Helpers
import Game.Font
import Game.Data.Color
import Game.Grid
import Game.Grid.Output
import Game.Run.RunData
import Game.Run.RunWorld
import Game.Run.RunWorld.OutputState
import Game.Run.Helpers
import Game.Run.Iteration.State
import Game.Run.Output.Fancy.ShadeCube
import Game.LevelPuzzle.Output.Fancy.ShadeDot
import Test.Tweak.State

import OpenGL
import OpenGL.Helpers


import OpenAL
import OpenAL.Helpers


--------------------------------------------------------------------------------
-- Begin

outputBegin :: RunWorld -> MEnv' RunWorld
outputBegin = \run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "iterationBegin"
      outputSoundBegin gamedata run
      outputScreenBegin gamedata run
    
    return run


outputTestTweak' :: TestTweakState -> RunWorld -> b -> MEnv' (TestTweakState, RunWorld, b)
outputTestTweak' = \s run b -> do
    gamedata <- resourceGameData 
    let Shape wth hth = sceneShape $ runScene run
        proj = mat4Perspective valuePerspectiveFOVY
                               (wth / hth)
                               valuePerspectiveNear 
                               valuePerspectiveFar
        view = cameraViewMat4 $ runCamera run
        model = cameraModelMat4 $ runCamera run
        modv = view `mappend` model
        normal = modv

    io $ do
        -- 3D --
        let projmodv = proj `mappend` modv
            tweak = sceneTweak $ runScene run
        glEnable gl_DEPTH_TEST
        glEnable gl_CULL_FACE

        -- space box
        let colormap = griddataColorMap $ gamedataGridData gamedata
            color = colormapAtSafe colormap $ ttstateColorIx s
        shadeSpaceColor (griddataShadeSpace $ gamedataGridData gamedata) tweak
                        1.0 modv color
        
        
        -- shadeMyDot --
        let sh = levelpuzzledataShadeDot $ gamedataLevelPuzzleData gamedata
            tweak = sceneTweak $ runScene run
            Vec4 fx fy fz fw = tweakForPos tweak
            Vec4 rx ry rz rw = tweakRotPos tweak 
            osc0 = tweakOsc0Pos tweak
            osc1 = tweakOsc1Pos tweak
            osc2 = tweakOsc2Pos tweak
            constl = tweakConstL tweak
        shadeDotBegin sh 1.0 projmodv normal
        shadeDotRadius sh 0.1
        shadeDotUseTexPlain sh

        -- origo
        shadeDotColor sh $ Color 1.0 1.0 1.0 1.0
        shadeDotPos sh 0.0 0.0 0.0

#ifdef GRID_TEST_FOR
        -- For
        shadeDotColor sh $ Color 1.0 0.0 0.0 1.0
        shadeDotPos sh fx fy fz
#endif

#ifdef GRID_TEST_ROT
        -- Rot
        shadeDotColor sh $ Color 0.0 1.0 0.0 1.0
        shadeDotPos sh rx ry rz
#endif

#ifdef GRID_TEST_OSC0
        -- Osc0
        shadeDotColor sh colorYellow
        shadeDotPos sh 0.0 0.0 (tweakConstL tweak * osc0)
#endif

#ifdef GRID_TEST_PULSE0
        shadeDotColor sh colorYellow
        shadeDotPos sh 0.0 0.0 (tweakPulse0Pos tweak)
#endif
        shadeDotEnd sh

        -- 2D --
        let Shape wth hth = sceneShape $ runScene run
            projmodv = mat4Ortho2D 0 wth hth 0
        glDisable gl_CULL_FACE

        -- Text
        drawText gamedata projmodv run $ [
            "ColorIx:        " ++ show (ttstateColorIx s)

            , "tweakConstI:    " ++ showFloat 3 (tweakConstI tweak)
            , "tweakConstJ:    " ++ showFloat 3 (tweakConstJ tweak)
            , "tweakConstK:    " ++ showFloat 3 (tweakConstK tweak)
            , "tweakConstL:    " ++ showFloat 3 (tweakConstL tweak)
#ifdef GRID_TEST_FOR
            , "tweakForPos:    " ++ showVec4 (tweakForPos tweak)
#endif
#ifdef GRID_TEST_ROT
            , "tweakRotPos:    " ++ showVec4 (tweakRotPos tweak)
#endif
#ifdef GRID_TEST_OSC0
            , "Osc0Pos:        " ++ showFloat 2 (tweakConstL tweak * tweakOsc0Pos tweak)
#endif
#ifdef GRID_TEST_PULSE0
            , "tweakPulse0Pos: " ++ showFloat 3 (tweakPulse0Pos tweak)
#endif
          ]

        return (s, run, b)


showVec4 :: Vec4 -> String
showVec4 (Vec4 x y z w) = 
    "(" ++ showFloat 2 x ++ " " ++ 
           showFloat 2 y ++ " " ++ 
           showFloat 2 z ++ " " ++
           showFloat 2 w ++ ")"

showFloat :: UInt -> Float -> String
showFloat dec = \v -> 
    printf ("%." ++ show dec ++ "f") v
    



-- | draw text
drawText :: GameData -> Mat4 -> RunWorld -> [ String ] -> IO ()
drawText gamedata projmodv run strs = do
    let fsh = gamedataFontShade gamedata        
        ffd = gamedataFontData gamedata
        Shape wth hth = sceneShape $ runScene run
    fontShade fsh 1.0 projmodv         
    fontDrawDefault fsh ffd (0.03 * hth) valueTextFontCColor 
    foldM_ (drawStr wth hth fsh ffd 0.03) 0.01 strs
    where
      drawStr wth hth fsh ffd x = \y str -> do
          fontDraw2D fsh ffd (x * wth) (y * hth) str
          return (y + 0.03) 


--------------------------------------------------------------------------------
--  helpers




shadeDotPos :: ShadeDot -> Float -> Float -> Float -> IO ()
shadeDotPos sh x y z = do
    glUniform3f (shadeDotUniPos sh) (rTF x) (rTF y) (rTF z)
    glDrawArrays gl_TRIANGLE_STRIP 0 $ fI $ valueLevelPuzzleDotStacks * 
                                            (2 * (valueLevelPuzzleDotSlices + 1) + 2)


fade :: Float -> Tick -> Tick -> Float
fade scale t0 t1 = 
    min 1.0 $ rTF (t1 - t0) * scale


projmodvRef :: Mat4 -> Segment -> Mat4
projmodvRef projmodv (Segment node turn) =
    mat4TranslateNode (mat4RotateTurn projmodv turn) node 


gridColor :: GameData -> UInt -> Color
gridColor gamedata ix = 
    colormapAt (griddataColorMap $ gamedataGridData gamedata) ix
    

