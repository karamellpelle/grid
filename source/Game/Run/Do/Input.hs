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
module Game.Run.Do.Input
  (
    inputRunTurnFace,
    inputRunTurn,
    inputTurn,

  ) where

import MyPrelude
import Game
import Game.Run.RunWorld
import Game.Run.Helpers



-- | control Turn
inputRunTurn :: RunWorld -> MEnv' RunWorld
inputRunTurn run = do
    maybeTurn <- inputTurn
    case maybeTurn of

        Nothing  -> do
            return run 
               
        Just t   -> do
            let turn' = t `mappend` runTurn run
            return $ runModifyCamera (run { runTurn = turn' }) $ \cam -> 
                     let View a1 b1 c1 = cameraCurrentView  $ runCamera run
                         -- bind ViewA and ViewB
                         a1' = keepInside (-valueRunGridViewABound) 
                                          (valueRunGridViewABound) $ moduloNegTauTau a1
                         b1' = keepInside (-valueRunGridViewBBound) 
                                          (valueRunGridViewBBound) $ moduloNegTauTau b1
                         view' = View a1' b1' c1
                     in  cameraToTurnView cam valueRunTurnSpeed turn' view'
    



-- | control Turn and Face
inputRunTurnFace :: RunWorld -> MEnv' RunWorld
inputRunTurnFace run = do
    maybeTurn <- inputTurn
    case maybeTurn of
        Nothing  -> keysTouchHandleButtonA run $ \_ -> 
           case faceFromTurn $ runTurn run of
               FaceLevelMode     -> worldPushEvent run EventLevelMode 
               FacePuzzleMode    -> worldPushEvent run EventPuzzleMode
               FaceMemoryMode    -> worldPushEvent run EventMemoryMode
               FaceForeign       -> worldPushEvent run EventForeign
               FaceAbout         -> worldPushEvent run EventAbout
               FaceSettings      -> worldPushEvent run EventSettings
               
        Just t   -> do
            let turn' = t `mappend` runTurn run
            return $ runModifyCamera (run { runTurn = turn' }) $ \cam -> 
                     let View a1 b1 c1 = cameraCurrentView  $ runCamera run
                         a1' = keepInside (-valueRunGridViewABound) 
                                          (valueRunGridViewABound) $ 
                                          moduloNegTauTau a1
                         b1' = keepInside (-valueRunGridViewBBound) 
                                          (valueRunGridViewBBound) $
                                          moduloNegTauTau b1
                         view' = View a1' b1' c1
                     in  cameraToTurnView cam valueRunTurnSpeed turn' view'
    



--------------------------------------------------------------------------------
--  

inputTurn :: MEnv' (Maybe Turn)
inputTurn = do

    keysTouchHandlePointDrop Nothing $ \_ (x, y) (x', y') -> 
        let dx = x' - x
            dy = y' - y
            maybe | abs dy < abs dx = maybeX
                  | abs dx < abs dy = maybeY
                  | otherwise       = Nothing
            maybeX | sens <= dx     = Just rightTurn
                   | dx <= (-sens)  = Just leftTurn
                   | otherwise      = Nothing
            maybeY | sens <= dy     = Just downTurn
                   | dy <= (-sens)  = Just upTurn
                   | otherwise      = Nothing
        in  maybe
    
    where
      sens = valueGridControlWaitSens
    


