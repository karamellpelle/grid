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
module Game.Run.Iteration.Konami
  (
    iterationDoEggKonami,

  ) where


import MyPrelude
import Game
import File

import Game.Grid.Helpers
import Game.Run.RunData
import Game.Run.RunWorld
import Game.Run.Do
import Game.Run.Output
import Game.Run.Scene
import Game.Run.Helpers

import OpenGL
import OpenGL.Helpers



--------------------------------------------------------------------------------
--  iterationDoEggKonami


iterationDoEggKonami :: Iteration' RunWorld
iterationDoEggKonami =
    iterationEggKonami


iterationEggKonami :: Iteration' RunWorld
iterationEggKonami =
    makeIteration' $ \run -> do 
       
        -- begin ShadeSceneKonami
        rundata <- resourceRunData
        io $ beginShadeSceneKonami $ rundataShadeSceneKonami rundata

        run' <- outputKonami run

        -- set Camera
        let run'' = setCamera run'
            
        iteration' (iterationEggKonami' ()) run''

        where
          setCamera run = 
              let run' = runSetTurnView run turnFaceLevelMode view
                  run'' = runSetCameraCmds run' [ camcmdWait t0,
                                                  camcmdViewAdd t1 s1 view1,
                                                  camcmdViewAdd t2 s2 view2 ]
              in  run''

          view = View 0.0 0.0 90.0
          t0 = 8.0
          t1 = 1.0 / rTF s1
          s1 = 0.001 * valueRunMessageSpeed
          view1 = View (2.0 * tau) 0.0 32.0
          t2 = 2.0
          s2 = 1.0 / rTF t2
          view2 = View 0.0 0.0 (-20.0)




iterationEggKonami' :: s -> Iteration' RunWorld
iterationEggKonami' s = 
    -- fixme: outputEggKonami'
    konamiIteration s outputMain' $ defaultStep doEmpty $ \s run b -> do
        
        if (gridCameraCmdsIsComplete $ runGrid run)
          then do
             
              -- send achievement
              playersSendAchievement valuePlayersAchievementEggKonami 1.0
             
              -- Message
              run' <- commentKonami $ setMessage run

              return (run', b, [])

          else do
              return (run, b, [iterationEggKonami' s])

    where
      -- fixme: clear message (path clear, message clear)
      setMessage run = 
          let run' = runMessageAbort run
              run'' = runSetMessageAtRef run' ref
          in  run''
          where
            r = fI valueRunMessageRadius
            ref = Segment (Node (-r) 0 (-r)) rightTurn


--------------------------------------------------------------------------------
--  

konamiIteration :: s -> 
              (s -> RunWorld -> () -> MEnv' (s, RunWorld, ())) -> 
              (s -> RunWorld -> () -> MEnv' (RunWorld, (), IterationStack RunWorld ())) ->
              Iteration' RunWorld
konamiIteration s output step =
    makeIteration' $ \run -> do

        -- handle screen change
        run' <- runBeginScreen run

        -- begin Scene
        scene' <- sceneBegin $ runScene run'

        -- actual iteration
        (run'', top) <- iteration' (defaultIteration s output step) $
                        run' { runScene = scene' }

        -- output Scene using RunWorld
        -- note: ideally, we should use run', since the output is of frame 
        --       n instead of frame (n + 1)
        let tweak = sceneTweak $ runScene run''
        scene'' <- scenePresentKonami (runScene run'') tweak run'' 
       
        
        -- save memory
        when (null top) $ do
            rundata <- resourceRunData
            io $ endShadeSceneKonami $ rundataShadeSceneKonami rundata


        return (run'' { runScene = scene'' }, top)

