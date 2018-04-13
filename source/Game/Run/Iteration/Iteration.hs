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
module Game.Run.Iteration.Iteration
  (
    beginIteration,
    mainIteration,
    modeIteration,

  ) where


import MyPrelude
import Game
import File

import Game.Run.RunData
import Game.Run.RunWorld
import Game.Run.Helpers.Make
import Game.Run.Iteration.Pause
import Game.Run.Do
import Game.Run.Scene
import Game.Run.Output
import Game.Run.Helpers

import OpenGL
import OpenGL.Helpers




--------------------------------------------------------------------------------
--  beginIteration

-- | iteration to Scene for iterationBegin
beginIteration :: 
        s -> 
        (s -> RunWorld -> () -> MEnv' (s, RunWorld, ())) -> 
        (s -> RunWorld -> () -> MEnv' (RunWorld, (), IterationStack RunWorld ())) -> 
        Iteration' RunWorld
beginIteration s output step =
    makeIteration' $ \run -> do
  
        -- handle screen change
        run' <- runBeginScreen run
      
        -- begin Scene
        let tick = worldTick run'
        scene' <- sceneBegin $ runScene run'

        -- actual iteration
        (run'', top) <- iteration' (defaultIteration s output step) $ 
                        run' { runScene = scene' }

        -- output Scene, using tick of RunWorld
        let tweak = sceneTweak $ runScene run''
        scene'' <- scenePresentBegin (runScene run'') tweak tick

        -- end resources (save memory)
        when (null top) $ do
            rundata <- resourceRunData
            io $ do
                -- this invalidates 'IterationBegin' part of SoundRun
                unloadSoundRunIterationBegin (rundataSoundRun rundata)
                -- this invalidates ShadeScene
                unloadShadeSceneBegin (rundataShadeSceneBegin rundata)


        return (run'' { runScene = scene'' }, top)


--------------------------------------------------------------------------------
--  mainIteration

-- | iteration to Scene for iterationMain
mainIteration :: 
        s -> 
        (s -> RunWorld -> () -> MEnv' (s, RunWorld, ())) -> 
        (s -> RunWorld -> () -> MEnv' (RunWorld, (), IterationStack RunWorld ())) -> 
        Iteration' RunWorld
mainIteration s output step =
    makeIteration' $ \run -> do
        -- handle screen change
        run' <- runBeginScreen run
       
        -- begin Scene
        scene' <- sceneBegin $ runScene run'

        -- actual iteration 
        (run'', top) <- iteration' (defaultIteration s output step) $
                        run' { runScene = scene' }
       
        -- output Scene
        let tweak = sceneTweak $ runScene run''
        scene'' <- scenePresent (runScene run'') tweak
        let run''' = run'' { runScene = scene'' }

        -- handle a new local player
        join $ playersHandleLocalPlayer (return (run''', top)) $ \player -> do

            path <- fileRunWorld player
           
            -- if local player does not exist, create data for it
            -- (copying the empty RunWorld)
            exists <- io $ doesFileExist path
            unless exists $ createLocalPlayerData player

            -- continue with RunWorld for that player
            run'''' <- reloadRunWorld run''' path
            gameSetIntensity $ runIntensity run''''

            -- say hi
            let run''''' = runMessagePush run'''' $ "wellcome, " ++ playerAlias player

            return (run''''', top)


--------------------------------------------------------------------------------
--  modeIteration


-- | iteration for modes
modeIteration :: 
      s -> 
      (s -> RunWorld -> () -> MEnv' (s, RunWorld, ())) -> 
      (s -> RunWorld -> () -> MEnv' (RunWorld, (), IterationStack RunWorld ())) -> 
      Iteration' RunWorld
modeIteration s output step =
    makeIteration' $ \run -> do

        -- handle screen change
        run' <- runBeginScreen run
        
        -- begin Scene
        scene' <- sceneBegin $ runScene run'

        -- actual iteration
        (run'', top) <- iteration' (defaultIteration s output step) $
                        run' { runScene = scene' }
      
        -- output Scene
        let tweak = sceneTweak $ runScene run''
        scene'' <- scenePresentMode (runScene run'') tweak
        let run''' = run'' { runScene = scene'' }

        -- question: handle low memory warning (save RunWorld)?

        -- handle frontground end (pause)
        join $ systemHandleFrontEnd (return (run''', top)) $ do

            -- save RunWorld
            saveRunWorld run'''

            -- iterationPause
            return (run''', [iterationPause] ++ top)



