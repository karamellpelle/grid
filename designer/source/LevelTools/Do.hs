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
module LevelTools.Do
  (
    doEdit,
    doTestLevel,
  ) where

import MyPrelude
import Game

import LevelTools.EditWorld
import Game.Run.RunWorld
import LevelTools.Helpers
import LevelTools.Do.Input
import LevelTools.Do.Grid
import Game.Helpers
import Game.Grid.Helpers


iterateWAStack = iterateABStack


doTestLevel s edit a = do
    
    (lvl', run', stack') <- iterateABStack (editLevelPuzzleWorld edit) 
                                                RunWorld (editLevelPuzzleStack edit)

    let edit' = edit
                {
                    editLevelPuzzleWorld = lvl',
                    editLevelPuzzleStack = stack'
                }
    return (s, edit', a)



doEdit :: s -> EditWorld -> b -> MEnv' (s, EditWorld, b)
doEdit s edit b = do
    edit' <- inputEdit edit


    -- camera
    let edit'' = editModifyCamera edit' $ \cam -> cameraToNode cam (editCameraNode edit)

    -- grid
    (s', grid', edit''') <- doGridEdit s (editGrid edit'') edit''
    let edit'''' = edit''' { editGrid = grid' }

    return (s', edit'''', b)



{-
modifyNode edit = io $ do
    
    left <- isPress (SpecialKey LEFT)
    right <- isPress (SpecialKey RIGHT)
    down <- isPress (SpecialKey PAGEDOWN)
    up <- isPress (SpecialKey PAGEUP)
    forward <- isPress (SpecialKey UP)
    backward <- isPress (SpecialKey DOWN)

    let maybeDiff | left  = Just $ Node 0 0 (-1)
                  | right = Just $ Node 0 0 1
                  | down  = Just $ Node 0 (-1) 0
                  | up    = Just $ Node 0 1 0
                  | forward = Just $ Node 1 0 0 
                  | backward = Just $ Node (-1) 0 0
                  | otherwise = Nothing

    case maybeDiff of
        Nothing   -> return edit
        Just diff -> do
            let node = diff `mappend` editNode edit
            return $ edit { editNode = node,
                            editGrid = gridCameraToNode (editGrid edit) 1 node }




modifyType edit = io $ do

    wall <- isPress (SpecialKey KP_0)
    simpledot <- isPress (SpecialKey KP_1)
    bonusdot <- isPress (SpecialKey KP_2)
    teledot <- isPress (SpecialKey KP_3)

    let maybeType | wall      = Just TypeWall
                  | simpledot = Just TypeSimpleDot
                  | bonusdot  = Just TypeBonusDot
                  | teledot   = Just TypeTeleDot
                  | otherwise = Nothing

    case maybeType of
        Nothing   -> return edit
        Just t    -> return $ edit { editType = t,
                                     editPushedNodes = [] }
            

modifyMaxSegments edit = io $ do
    plus <- isPress (SpecialKey INSERT)
    minus <- isPress (SpecialKey DEL)

    case plus of
        True    -> return $ edit { editLevelMaxSegments = (editLevelMaxSegments edit) + 1}
        False   -> case minus of
            True  -> return $ edit { editLevelMaxSegments = (editLevelMaxSegments edit) - 1 }
            False -> return edit


modifyTypeValue edit = io $ do
    plus <- isPress (SpecialKey HOME)
    minus <- isPress (SpecialKey END)

    case plus of
        True    -> return $ edit { editTypeValue = (editTypeValue edit) + 1 }
        False   -> case minus of
            True  -> return $ edit { editTypeValue = (editTypeValue edit) - 1 }
            False -> return edit


modifyEatOrd edit = io $ do
    plus <- isPress (CharKey 'A')
    minus <- isPress (CharKey 'Z')
    toggle <- isPress (CharKey '<')

    case toggle of
        True    -> return $ edit { editIsEatOrd = not $ editIsEatOrd edit }
        False   -> case plus of
            True    -> return $ edit { editEatOrd = editEatOrd edit + 1 }
            False   -> case minus of
                True    -> return $ edit { editEatOrd = editEatOrd edit - 1 }
                False   -> return edit

modifyPushNode edit = io $ do
    pushNode <- isPress (SpecialKey ENTER)
    case pushNode of
        False   -> return edit
        True    -> let edit' = edit { editPushedNodes = editPushedNodes edit ++ [editNode edit] }
                   in case editType edit' of
                      TypeWall      -> return $ wallHandlePush edit'
                      TypeSimpleDot -> return $ simpledotHandlePush edit'
                      TypeBonusDot  -> return $ bonusdotHandlePush edit'
                      TypeTeleDot   -> return $ teledotHandlePush edit'


-- view abc
modifyCamera edit = io $ do
    Size wth hth <- get windowSize
    Position x y <- get mousePos
    let x' = fromIntegral (x - wth `div` 2) / fromIntegral (wth `div` 2)
        y' = fromIntegral (y - hth `div` 2) / fromIntegral (hth `div` 2)

    return edit -- fixme

modifyErase edit = io $ do
    erase <- isPress (SpecialKey BACKSPACE)
    case erase of
        False -> return edit
        True  ->
          let node = editNode edit
          in  case eraseWall edit node of
              (edit', True)   -> return edit' 
              (edit', False)  -> case eraseDot edit node of
                  (edit'', _)  -> return edit''



--------------------------------------------------------------------------------
--  

wallHandlePush edit =
    case editPushedNodes edit of
        (n0:n1:n2:ns) -> 
            let node = n0
                x = (diffNode n0 n1)
                y = (diffNode n0 n2)
                double = editTypeValue edit /= 0
                wall = Wall node x y double

            in  edit { editLevelWalls = editLevelWalls edit ++ [wall],
                       editPushedNodes = [] }

        _             -> edit


simpledotHandlePush edit = 
    case editPushedNodes edit of
        (n0:ns) -> 
            let node = n0
                dot = if editIsEatOrd edit then makeSimpleDot' (editEatOrd edit) node else makeSimpleDot node
            in  edit { editLevelDots = editLevelDots edit ++ [dot],
                       editPushedNodes = [] }

        _       -> edit


bonusdotHandlePush edit = 
    case editPushedNodes edit of
        (n0:ns) -> 
            let node = n0
                value = editTypeValue edit
                dot = if editIsEatOrd edit 
                      then makeBonusDot' (editEatOrd edit) node value 
                      else makeBonusDot node value

            in  edit { editLevelDots = editLevelDots edit ++ [dot],
                       editPushedNodes = [] }

        _       -> edit

teledotHandlePush edit = 
    case editPushedNodes edit of
        (n0:n1:ns) -> 
            let node = n0
                node' = n1
                dot = if editIsEatOrd edit
                      then makeTeleDot' (editEatOrd edit) node node'
                      else makeTeleDot node node'
            in  edit { editLevelDots = editLevelDots edit ++ [dot],
                       editPushedNodes = [] }

        _         -> edit

eraseWall edit node = 
    case helper node (editLevelWalls edit) of
        (walls', False) -> (edit { editLevelWalls = walls' }, False)
        (walls', True)  -> (edit { editLevelWalls = walls' }, True)
    where
      helper node (w:ws) =
        if (wallNode w == node) 
          then (ws, True)
          else let (ws', bool) = helper node ws
               in  (w:ws', bool)

      helper node [] =
          ([], False) 


eraseDot edit node = 
    case helper node (editLevelDots edit) of
        (dots', False) -> (edit { editLevelDots = dots' }, False)
        (dots', True)  -> (edit { editLevelDots = dots' }, True)
    where
      helper node (w:ws) =
        if (dotNode w == node) 
          then (ws, True)
          else let (ws', bool) = helper node ws
               in  (w:ws', bool)

      helper node [] =
          ([], False)        


--------------------------------------------------------------------------------
--  


isPress key = io $
    getKey key >>= \state -> case state of
        Release    -> return False
        Press      -> return True
-}
