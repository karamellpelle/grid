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
module LevelTools.Iteration
  (
    iterationEdit,
    iterationTestLevel,

  ) where

import MyPrelude
import Game

import LevelTools.Output
import LevelTools.Do
import LevelTools.Helpers
import LevelTools.EditWorld
import LevelTools.Make

import Game.Run.RunWorld

import Game.LevelPuzzleMode.Iteration   as Level
import Game.LevelPuzzleMode.LevelPuzzleWorld.Make

-- tmp:
import Graphics.UI.GLFW


iterationEdit :: Iteration' EditWorld
iterationEdit =
    makeIteration' $ \edit -> do
        io $ putStrLn "iterationEdit"

        tickClockFSet $ worldTick edit
        let edit' = edit { editMessage = [] }

        iteration' (iterationEdit' ()) edit'


iterationEdit' :: s -> Iteration' EditWorld 
iterationEdit' s =
    defaultIteration s outputEdit $ defaultStep doEdit $ \s edit b -> do

        pressLevel <- keysKeyOnce (SpecialKey TAB)
        pressExit <- keysKeyOnce (SpecialKey ESC)
        pressCtrl <- getkey (SpecialKey LCTRL)
        
        case pressLevel of

            -- test level (if valid)
            True  -> do
                case findInvalidObject edit of
                    Nothing                 -> 
                        return (edit, b, [iterationTestLevel pressCtrl, iterationEdit' s])

                    Just (room, node, str)  -> do
                          let edit' = editModifySemiContent edit $ \cnt -> cnt { scontentRoom = room }
                              edit'' = editChangeObject edit' node
                              edit''' = edit''
                                       {
                                          editNode = node,
                                          editCameraNode = node,
                                          editMessage = str
                                       }
                          return (edit''', b, [iterationEdit' s])
                           

            False -> case pressExit of

                -- exit
                True  -> do
                    return (edit, b, [])

                False -> do
                    return (edit, b, [iterationEdit' s])
    where
      getkey key = io $ 
          getKey key >>= \state -> case state of
              Release   -> return False
              Press     -> return True
{-
-- | validate all objects (unless escape)
iterationValidate :: Iteration' EditWorld
iterationValidate =
    makeIteration' $ \edit -> do
        io $ putStrLn "iterationValidateObjects"

        tickClockFSet $ worldTick edit
        let edit' = worldPushEvent (edit { editMessage = [] }) EventValidate
        
        iteration' (iterationValidate' False) edit'

iterationValidate' :: (Bool, Bool) -> Iteration' EditWorld 
iterationValidate' mc =
    defaultIteration mc outputValidate $ defaultStep doEdit $ \mc edit b -> do
       
        (edit', b', top') <- handleAllEventsM edit (edit, b, [iterationValidate' mc]) $ \ebt@(edit, b, top) event -> case event of
            EventValidate -> do
                case findInvalidObject edit of
                    Nothing             -> do
                        let edit' = edit { editMessage = "validation complete" } 
                        return (edit', b, [iterationValidate' (True, True) ])

                    Just (node, room)   -> do
                        let edit' = editModifySemiContent edit $ \cnt -> 
                                    cnt { scontentRoom = room }
                            edit'' = edit'
                                     {
                                        editNode = node,
                                        editCameraNode = node,
                                        editMessage = "invalid RoomIx: " ++ show room
                                     }
                        return (edit'', b, top) 


            _             -> return ebt
          

        let (mayEscape, complete) = mc
        if mayEscape 
          then do
            pressLevel <- keysKeyOnce (SpecialKey TAB)
            pressExit <- keysKeyOnce (SpecialKey ESC)
            if pressLevel || pressExit then return (edit', b', [])
                                       else return (edit', b', top')
          else do
            pressLevel <- keysKeyOnce (SpecialKey TAB)
            pressExit <- keysKeyOnce (SpecialKey ESC)
            if pressLevel || pressExit && not complete 
              then do
                let edit'' = edit { editMessage = "Warning: " }
                return (edit'', b', [iterationValidate' (True, complete)]) 
              else do
                return (edit'', b', [iteration

            return (edit', b', 
            pressExit <- keysKeyOnce (
            return (edit', b', top')
            -- 
-}

iterationTestLevel force = 
    makeIteration' $ \edit -> do
        io $ putStrLn "iterationTestLevel"

        if editLevelPuzzleEmpty edit || force
          then do
            unloadLevelPuzzleWorld $ editLevelPuzzleWorld edit
            lvl <- lvlFromEdit edit
            
            let lvl' = lvl { levelpuzzleIsPuzzle = True }
            tickClockLevelPuzzleModeSet $ worldTick lvl'

            let run = RunWorld
            iteration' (iterationTestLevel' ()) $ edit
                                                  {
                                                      editLevelPuzzleEmpty = False,
                                                      editLevelPuzzleWorld = lvl',
                                                      editLevelPuzzleStack = [Level.iterationBeginPlay]
                                                  }

          else do
            io $ putStrLn $ "fixme: update difference EditWorld -> LevelPuzzleWorld when toggle Play!"
            -- fixme: difference levelpuzzle -> edit
            --tickClockLevelPuzzleModeSet $ worldTick lvl
            iteration' (iterationTestLevel' ()) $ edit
        


iterationTestLevel' lrs = 
    defaultIteration lrs noOutput $ defaultStep doTestLevel $ \s edit b -> do
       
      
        pressExit <- keysKeyOnce (SpecialKey TAB)
        case pressExit of
            False   -> return (edit, b, [iterationTestLevel' s])
            True    -> return (edit, b, [])
        

--------------------------------------------------------------------------------
--  

