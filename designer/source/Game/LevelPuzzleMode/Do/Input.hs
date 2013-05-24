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
module Game.LevelPuzzleMode.Do.Input
  (
    inputSpecialComplete,

  ) where

import MyPrelude
import Game
import Game.LevelPuzzleMode.LevelPuzzleWorld
import Game.LevelPuzzleMode.Helpers
import Game.LevelPuzzleMode.Iteration.State

import Game.Run.RunWorld

import Game.Grid.Helpers


-- | control cube turn and face
inputSpecialComplete :: SpecialCompleteS -> LevelPuzzleWorld -> RunWorld -> 
                        MEnv' (SpecialCompleteS, LevelPuzzleWorld, RunWorld)
inputSpecialComplete s lvl run = do
    return (s, lvl, run)
{-
    maybeTurn <- inputTurn
    case maybeTurn of
        Nothing  -> do
            return (s, lvl, run)
               
        Just t   -> do
            let turn' = t `mappend` specialcompletesTurn s
                lvl' = levelpuzzleModifyCamera lvl $ \cam -> 
                       let View a1 b1 c1 = cameraCurrentView  $ levelpuzzleCamera lvl
                           a1' = keepInside (-valueRunGridViewABound) 
                                            (valueRunGridViewABound) $ 
                                            moduloNegTauTau a1
                           b1' = keepInside (-valueRunGridViewBBound) 
                                            (valueRunGridViewBBound) $
                                            moduloNegTauTau b1
                           view' = View a1' b1' c1
                       in  cameraToTurnView cam valueRunFaceSpeed turn' view'
                s' = s { specialcompletesTurn = turn' }

            return (s', lvl', run) 
-}



