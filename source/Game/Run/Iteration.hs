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
module Game.Run.Iteration
  (
    iterationBegin,
    iterationMain,
    iterationEnd,

  ) where


import MyPrelude
import Game

import Game.Run
import Game.Run.Output
import Game.Run.Do

import Game.Run.Iteration.Iteration
import Game.Run.Iteration.LevelPuzzle
import Game.Run.Iteration.Memory
import Game.Run.Iteration.Foreign
import Game.Run.Iteration.About
import Game.Run.Iteration.Settings
import Game.Run.Iteration.AtFace
import Game.Run.Iteration.Konami



--------------------------------------------------------------------------------
-- iterationBegin: begin game


iterationBegin :: Iteration' RunWorld
iterationBegin = 
    makeIteration' $ \run -> do

        -- set clocks to 0.0
        tickSet 0.0
        tickClockRunSet 0.0
        tickClockLevelPuzzleSet 0.0
        tickClockMemorySet 0.0

        -- output
        run' <- outputBegin run
        
        -- show message
        run'' <- commentIterationBegin $ setMessage run'
      
        -- set up Camera
        (wth, hth) <- screenShape
        let run''' = setCamera wth hth run''
        
        -- speed up iterationBegin
        tickClockRunSetSpeed valueIterationBeginSpeed

        iteration' (iterationBegin' ()) run'''

-- ignore intro:
{-
        gameSetIntensity 0.0
        iteration' (iterationMain) $ runSetCameraCmds run'
                                     [ camcmdView 0.0 1.0 valueRunGridView]
-}
    where
      setCamera wth hth run = 
          let run' = runSetTurnView run turnFaceForeign $ viewFromShape (Shape wth hth) 
              run'' = runSetCameraCmds run' [  camcmdWait t0,
                                               camcmdWait t1,
                                               camcmdViewAdd t2 s2 view2,
                                               camcmdViewAdd t3 s3 view3,
                                               camcmdViewAdd t4 s4 view4 ]
          in  run''

      setMessage run =
          runMessagePush (runSetMessageAtRef run ref) $ replicate 240 '\0'
          where 
            ref = Segment (Node 0 0 (fI valueRunMessageRadius)) straightTurn
      
      t0 = valueRunIterationBeginT0
      t1 = valueRunIterationBeginT1
      t2 = valueRunIterationBeginT2
      s2 = valueRunIterationBeginS2
      view2 = View 0.0 0.0 40.0
      t3 = valueRunIterationBeginT3
      s3 = valueRunIterationBeginS3
      view3 = View (-4.712) 0.2 20.0
      t4 = valueRunIterationBeginT4
      s4 = valueRunIterationBeginS4
      view4 = View 0.0 0.0 0.0

      valueIterationBeginSpeed = 5.0


iterationBegin' :: s -> Iteration' RunWorld
iterationBegin' s =
    beginIteration s outputBegin' $ defaultStep doEmpty $ \s run b -> do
        
        if (gridCameraCmdsIsComplete $ runGrid run)

          -- iterationBegin finished => iterationMain
          then do 

              -- set intensity
              gameSetIntensity $ runIntensity run

              let  view = cameraCurrentView $ runCamera run
                   run' = runSetTurnView run turnFaceLevelMode $ view { viewA = 0.0 }

              return (run', b, [iterationMain])
          
          else return (run, b, [iterationBegin' s])



--------------------------------------------------------------------------------
--  iterationEnd: end game


iterationEnd :: Iteration' RunWorld
iterationEnd = 
    makeIteration' $ \run -> do

        run' <- outputEnd run

        iteration' (iterationEnd' ()) run'


iterationEnd' :: s -> Iteration' RunWorld
iterationEnd' s = 
    makeIteration' $ \run -> do
        
        return (run, [])




--------------------------------------------------------------------------------
--  iterationMain: "main menu"


iterationMain :: Iteration' RunWorld
iterationMain = 
    makeIteration' $ \run -> do

        run' <- outputMain run

        iteration' (iterationMain' ()) run'


iterationMain' :: s -> Iteration' RunWorld
iterationMain' s = 
    mainIteration s outputMain' $ defaultStep doMain $ \s run b -> do

        -- now look at events!
        return $ handleAllEvents run (run, b, [iterationMain' s]) 
               $ \rbt@(run, b, top) event -> case event of

                  EventLevelMode        -> 
                      (run, b, [iterationDoLevelMode, iterationMain])
    
                  EventPuzzleMode       -> 
                      (run, b, [iterationDoPuzzleMode, iterationMain])

                  EventMemoryMode       -> 
                      (run, b, [iterationDoMemoryMode, iterationMain])

                  EventForeign          -> 
                      (run, b, [iterationDoForeign, iterationMain])

                  EventAbout            -> 
                      (run, b, [iterationDoAbout, iterationMain])

                  EventSettings         -> 
                      (run, b, [iterationDoSettings, iterationMain])
                  
                  EventEggKonami        -> 
                      (run, b, [iterationDoEggKonami, iterationMain])
                    
                  _                     -> 
                      rbt


--------------------------------------------------------------------------------
--  iterationDo

iterationDoLevelMode :: Iteration' RunWorld
iterationDoLevelMode =
    makeIteration' $ \run -> do
        iteration' (iterationAtFace FaceLevelMode [iterationLevelMode]) run


iterationDoPuzzleMode :: Iteration' RunWorld
iterationDoPuzzleMode =
    makeIteration' $ \run -> do
        iteration' (iterationAtFace FacePuzzleMode [iterationPuzzleMode]) run


iterationDoMemoryMode :: Iteration' RunWorld
iterationDoMemoryMode =
    makeIteration' $ \run -> do
        iteration' (iterationAtFace FaceMemoryMode [iterationMemoryMode]) run


iterationDoForeign :: Iteration' RunWorld
iterationDoForeign =
    makeIteration' $ \run -> do
        iteration' (iterationAtFace FaceForeign [iterationForeign]) run


