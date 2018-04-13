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
module Test.GL.Output
  (
    outputTestGL',
  
  ) where


import MyPrelude
import Game

import Game.Grid
import Game.Grid.Output
import Game.Run
import Game.Run.Output.Fancy.ShadeCube
import Game.LevelPuzzle.Output.Fancy.ShadeDot
import Game.Grid.Output.Fancy.ShadeGeneral
import Test.GL.State
import Test.Output

import OpenGL
import OpenGL.Helpers


import OpenAL
import OpenAL.Helpers



outputTestGL' :: TestGLState -> RunWorld -> b -> MEnv' (TestGLState, RunWorld, b)
outputTestGL' = \s run b -> do
    gamedata <- resourceGameData 
    let Shape wth hth = sceneShape $ runScene run
        proj = sceneProj3D $ runScene run
        view = cameraViewMat4 $ runCamera run
        model = cameraModelMat4 $ runCamera run
        modv = view `mappend` model
        normal = modv

#ifdef GRID_TEST_NO_SCENE
    fbo <- screenFBO
    (swth, shth) <- screenSize
    io $ do
        -- without Scene:
        glBindFramebuffer gl_FRAMEBUFFER fbo
        glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT .|. gl_STENCIL_BUFFER_BIT
        glViewport 0 0 (fI swth) (fI shth)
#else
    io $ do
#endif
        let projmodv = proj `mappend` modv
            cammodel = mat4CamModel model 
            tweak = sceneTweak $ runScene run
              
        -- ShadeSpace --
        let sh = griddataShadeSpace $ gamedataGridData gamedata 
        shadeSpaceColor sh tweak 1.0 modv $ Color 0.0 0.0 0.0 1.0 

        -- ShadeCube --
        let sh = rundataShadeCube $ gamedataRunData gamedata
            projmodv' = projmodv --mat4Scale (0.5 / valueRunCubeRadius) projmodv
            normal' = normal
        shadeCube sh 1.0 projmodv' normal' run

        -- ShadeDot --
        let sh = levelpuzzledataShadeDot $ gamedataLevelPuzzleData gamedata
        shadeDotBegin sh 1.0 projmodv cammodel
        shadeDotRadius sh 0.1
        shadeDotUseTexPlain sh 
        shadeDotColor sh colorRed
        shadeDotNode sh (Node 0 0 1)
        shadeDotColor sh colorGreen
        shadeDotNode sh (Node 0 0 2)
        shadeDotColor sh colorBlue
        shadeDotNode sh (Node 0 0 3)
        shadeDotColor sh colorNull
        shadeDotRadius sh 0.2
        shadeDotUseTexBonus sh
        shadeDotNode sh (Node 0 1 0)
        shadeDotUseTexTele0 sh
        shadeDotNode sh (Node 0 2 0)
        shadeDotUseTexTele1 sh
        shadeDotNode sh (Node 0 3 0)
        shadeDotUseTexFinish sh
        shadeDotNode sh (Node 0 4 0)
        shadeDotEnd sh

        -- ShadePath --
        let sh = griddataShadePath $ gamedataGridData gamedata
        shadePathBegin sh 1.0 projmodv
        shadePathColor sh colorYellow
        shadePathRadius sh 0.20
        --shadePathDraw sh $ tglstatePath s
        shadePathDrawLine sh 1 0 0 2 0 0
        shadePathEnd sh

        -- ShadeCorners --
        let sh = griddataShadeGeneral $ gamedataGridData gamedata
            cornerdata = rundataCornerData $ gamedataRunData gamedata
        shadeGeneral sh 1.0 $ sceneProj2D (runScene run)
        glActiveTexture gl_TEXTURE0
        glBindTexture gl_TEXTURE_2D $ cornerdataTex cornerdata
        glDisable gl_CULL_FACE
        glBindVertexArrayOES $ cornerdataVAO2D cornerdata
        glDrawArrays gl_TRIANGLE_STRIP 0 24
        glEnable gl_CULL_FACE

        -- FontShade --
        glDisable gl_CULL_FACE
        drawText gamedata wth hth [ "hello there, ", "Haskell!" ]
        glEnable gl_CULL_FACE
       
        -- return  
        return (s, run, b)
        


mat4CamModel :: Mat4 -> Mat4
mat4CamModel mat =
    (Mat4 0.0 0.0 (-1.0) 0.0
          0.0 1.0 0.0 0.0
          1.0 0.0 0.0 0.0
          0.0 0.0 0.0 1.0) `mappend` mat


mat4Scale :: Float -> Mat4 -> Mat4
mat4Scale a (Mat4 a0 a1 a2 a3
                  b0 b1 b2 b3
                  c0 c1 c2 c3
                  d0 d1 d2 d3) =
    Mat4 (a * a0) (a * a1) (a * a2) (a3)
         (a * b0) (a * b1) (a * b2) (b3)
         (a * c0) (a * c1) (a * c2) (c3)
         (d0)     (d1)     (d2)     (d3)

