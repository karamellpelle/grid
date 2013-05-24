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
module Game.Run.Eggs.Do
  (
    doEggs,

  ) where


import MyPrelude
import Game

import Game.Run.RunWorld
import Game.Run.Helpers
import Game.Run.Do.Input

import Game.Run.Eggs
import Game.Run.Eggs.SequenceEater
import Game.Run.Eggs.Button

  
--------------------------------------------------------------------------------
--  

doEggs :: RunWorld -> MEnv' RunWorld
doEggs run = do
    maybeTurn <- inputTurn
    maybeBut <- case maybeTurn of
                Nothing -> do
                    maybeB <- keysTouchHandleButtonB Nothing $ \_ -> Just ButtonB
                    keysTouchHandleButtonA maybeB $ \_ -> Just ButtonA
                Just t  -> case direction t of
                           Dir 0 1 0    -> return $ Just ButtonUp
                           Dir 0 (-1) 0 -> return $ Just ButtonDown
                           Dir 0 0 1    -> return $ Just ButtonRight
                           Dir 0 0 (-1) -> return $ Just ButtonLeft
                           _            -> return $ Nothing
    
    -- if there is a new button, eat it
    case maybeBut of
        Nothing   -> return run
        Just but  -> let run' = eatKonami run but
                         run'' = eatMessage run' but
                     in  return run''


eatKonami :: RunWorld -> EggButton -> RunWorld
eatKonami run but =
    case sequenceEat (runEggKonami run) but of
        (konami', False)  -> run { runEggKonami = konami' }

        (konami', True)   -> worldPushEvent (run { runEggKonami = konami' }) EventEggKonami


eatMessage :: RunWorld -> EggButton -> RunWorld
eatMessage run but = 
    case sequenceEat (runEggMessage run) but of
        (message', False)  -> run { runEggMessage = message' }

        (message', True)   -> runMessagePush (run { runEggMessage = message' }) $ 
                              "open source is your friend"
  
  

