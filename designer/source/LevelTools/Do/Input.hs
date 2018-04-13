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
module LevelTools.Do.Input
  (
      inputEdit,
  ) where

import MyPrelude
import Game

import LevelTools.EditWorld
import Game.LevelPuzzleMode.LevelPuzzleWorld
import LevelTools.Helpers
import Graphics.UI.GLFW


arrKey key arr = \a -> 
    keysKeyOnce key >>= \bool -> case bool of
        False   -> return a
        True    -> arr a

arrWhen :: Monad m => m Bool -> (a -> m a) -> a -> m a
arrWhen mbool arr = \a -> 
    mbool >>= \bool -> case bool of
        False   -> return a
        True    -> arr a

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mbool mthen melse =
    mbool >>= \bool -> case bool of
        False   -> mthen
        True    -> melse

myPred n = 
    if n == 0 then n
              else n - 1

mySucc n =
    n + 1


inputEdit =
    inputGeneral>>>
    inputCamera >>>
    inputNode   >>>
    inputRoom   >>>
    inputType   >>>
    inputObject


inputGeneral = 
    onType      >>>
    onIsPuzzle  >>>
    onSegments
    where
      onType = 
          (arrKey (SpecialKey F1) $ \edit ->
              return edit { editEditType = TypeDotPlain }) >>>
          (arrKey (SpecialKey F2) $ \edit -> 
              return edit { editEditType = TypeDotBonus }) >>>
          (arrKey (SpecialKey F3) $ \edit -> 
              return edit { editEditType = TypeDotTele }) >>>
          (arrKey (SpecialKey F4) $ \edit -> 
              return edit { editEditType = TypeDotFinish }) >>>
          (arrKey (SpecialKey F10) $ \edit -> 
              return edit { editEditType = TypeWall })
      
      onIsPuzzle = 
          (arrKey (CharKey 'Q') $ \edit -> 
              return $ editModifyLevel edit $ \level -> level { levelPuzzleTag = not (levelPuzzleTag level) })

      onSegments = 
          (arrKey (CharKey 'Y') $ \edit -> 
              return $ editModifyLevel edit $ \level -> level { levelSegments = myPred (levelSegments level) }) >>>
          (arrKey (CharKey 'U') $ \edit -> 
              return $ editModifyLevel edit $ \level -> level { levelSegments = mySucc (levelSegments level) })
      
inputCamera = 
    onNode >>>
    onView
    where
      onNode = 
          (arrKey (CharKey 'A') $ \edit -> 
              return $ editModifyCameraNode edit $ \(Node x y z) -> Node x y (z - 1)) >>>
          (arrKey (CharKey 'D') $ \edit -> 
              return $ editModifyCameraNode edit $ \(Node x y z) -> Node x y (z + 1)) >>>
          (arrKey (CharKey 'S') $ \edit -> 
              return $ editModifyCameraNode edit $ \(Node x y z) -> Node (x - 1) y z) >>>
          (arrKey (CharKey 'W') $ \edit -> 
              return $ editModifyCameraNode edit $ \(Node x y z) -> Node (x + 1) y z) >>>
          (arrKey (CharKey 'C') $ \edit -> 
              return $ editModifyCameraNode edit $ \(Node x y z) -> Node x (y - 1) z) >>>
          (arrKey (CharKey 'X') $ \edit -> 
              return $ editModifyCameraNode edit $ \(Node x y z) -> Node x (y + 1) z)

      onView edit =
          return edit


inputNode = 
    (arrKey (SpecialKey LEFT) $ \edit -> 
        return $ editModifyNode edit $ \(Node x y z) -> Node x y (z - 1)) >>>
    (arrKey (SpecialKey RIGHT) $ \edit -> 
        return $ editModifyNode edit $ \(Node x y z) -> Node x y (z + 1)) >>>
    (arrKey (SpecialKey DOWN) $ \edit -> 
        return $ editModifyNode edit $ \(Node x y z) -> Node (x - 1) y z) >>>
    (arrKey (SpecialKey UP) $ \edit -> 
        return $ editModifyNode edit $ \(Node x y z) -> Node (x + 1) y z) >>>
    (arrKey (SpecialKey PAGEDOWN) $ \edit -> 
        return $ editModifyNode edit $ \(Node x y z) -> Node x (y - 1) z) >>>
    (arrKey (SpecialKey PAGEUP) $ \edit -> 
        return $ editModifyNode edit $ \(Node x y z) -> Node x (y + 1) z)
   

inputRoom =
    (arrKey (CharKey '0') $ \edit -> 
        return $ editModifySemiContent edit $ \cnt -> cnt { scontentRoom = 0 }) >>>
    (arrKey (CharKey '1') $ \edit -> 
        return $ editModifySemiContent edit $ \cnt -> cnt { scontentRoom = 1 }) >>>
    (arrKey (CharKey '2') $ \edit -> 
        return $ editModifySemiContent edit $ \cnt -> cnt { scontentRoom = 2 }) >>>
    (arrKey (CharKey '3') $ \edit -> 
        return $ editModifySemiContent edit $ \cnt -> cnt { scontentRoom = 3 }) >>>
    (arrKey (CharKey '4') $ \edit -> 
        return $ editModifySemiContent edit $ \cnt -> cnt { scontentRoom = 4 }) >>>
    (arrKey (CharKey '5') $ \edit -> 
        return $ editModifySemiContent edit $ \cnt -> cnt { scontentRoom = 5 }) >>>
    (arrKey (CharKey '6') $ \edit -> 
        return $ editModifySemiContent edit $ \cnt -> cnt { scontentRoom = 6 }) >>>
    (arrKey (CharKey '7') $ \edit -> 
        return $ editModifySemiContent edit $ \cnt -> cnt { scontentRoom = 7 }) >>>
    (arrKey (CharKey '8') $ \edit -> 
        return $ editModifySemiContent edit $ \cnt -> cnt { scontentRoom = 8 }) >>>
    (arrKey (CharKey '9') $ \edit -> 
        return $ editModifySemiContent edit $ \cnt -> cnt { scontentRoom = 9 }) >>>
    (arrKey (CharKey 'R') $ \edit -> 
        return $ editModifySemiContent edit $ \cnt -> cnt { scontentRoom = myPred (scontentRoom cnt) }) >>>
    (arrKey (CharKey 'T') $ \edit -> 
        return $ editModifySemiContent edit $ \cnt -> cnt { scontentRoom = mySucc (scontentRoom cnt) })
   

inputType = \edit -> case editEditType edit of
    TypeDotPlain  -> onDotPlain edit
    TypeDotBonus  -> onDotBonus edit
    TypeDotTele   -> onDotTele  edit
    TypeDotFinish -> onDotFinish edit
    TypeWall      -> onWall edit
    _             -> return edit

    where
      onDotPlain = 
          (arrKey (SpecialKey ENTER) $ \edit ->
              return $ editPushDotPlain edit (editNode edit)) >>>
          (arrKey (CharKey 'L') $ \edit ->
              return $ editModifyDotPlain edit $ \dot -> dot { dotplainSize = myPred (dotplainSize dot) }) >>>
          (arrKey (CharKey 'O') $ \edit ->
              return $ editModifyDotPlain edit $ \dot -> dot { dotplainSize = mySucc (dotplainSize dot) }) >>>
          (arrKey (CharKey 'K') $ \edit ->
              return $ editModifyDotPlain edit $ \dot -> dot { dotplainRoom = myPred (dotplainRoom dot) }) >>>
          (arrKey (CharKey 'I') $ \edit ->
              return $ editModifyDotPlain edit $ \dot -> dot { dotplainRoom = mySucc (dotplainRoom dot) })
   
      onDotBonus = 
          (arrKey (SpecialKey ENTER) $ \edit ->
              return $ editPushDotBonus edit (editNode edit)) >>>
          (arrKey (CharKey 'L') $ \edit ->
              return $ editModifyDotBonus edit $ \dot -> dot { dotbonusSize = myPred (dotbonusSize dot) }) >>>
          (arrKey (CharKey 'O') $ \edit ->
              return $ editModifyDotBonus edit $ \dot -> dot { dotbonusSize = mySucc (dotbonusSize dot) }) >>>
          (arrKey (CharKey 'K') $ \edit ->
              return $ editModifyDotBonus edit $ \dot -> dot { dotbonusAdd = myPred (dotbonusAdd dot) }) >>>
          (arrKey (CharKey 'I') $ \edit ->
              return $ editModifyDotBonus edit $ \dot -> dot { dotbonusAdd = mySucc (dotbonusAdd dot) })

      onDotTele = 
          (arrKey (SpecialKey ENTER) $ \edit ->
              return $ editPushDotTele edit (editNode edit)) >>>
          (arrKey (CharKey 'L') $ \edit ->
              return $ editModifyDotTele edit $ \dot -> dot { dotteleSize = myPred (dotteleSize dot) }) >>>
          (arrKey (CharKey 'O') $ \edit ->
              return $ editModifyDotTele edit $ \dot -> dot { dotteleSize = mySucc (dotteleSize dot) })
     
      onDotFinish = 
          (arrKey (SpecialKey ENTER) $ \edit ->
              return $ editPushDotFinish edit (editNode edit))
      
      onWall = 
          (arrKey (SpecialKey ENTER) $ \edit ->
              return $ editPushWall edit (editNode edit)) >>>
          (arrKey (CharKey 'P') $ \edit -> 
              return $ editModifyWall edit $ \wall -> wall { wallIsDouble = not (wallIsDouble wall) })


      
inputObject = 
    (arrKey (CharKey 'M') $ \edit ->
        return $ editChangeObject edit (editNode edit)) >>>
    (arrKey (SpecialKey BACKSPACE) $ \edit -> 
        return $ editEraseObject edit (editNode edit))



    

--------------------------------------------------------------------------------
--  


editPushDotPlain edit node = 
    let dot = editDotPlain edit
        dot' = dot { dotplainNode = node }
        edit' = editModifySemiContent edit $ \cnt -> scontentPushDotPlain cnt (scontentRoom cnt) dot'
    in  edit' { editDotPlain = dot', editPushedNodes = [] }

editPushDotBonus edit node = 
    let dot = editDotBonus edit
        dot' = dot { dotbonusNode = node }
        edit' = editModifySemiContent edit $ \cnt -> scontentPushDotBonus cnt (scontentRoom cnt) dot'
    in  edit' { editDotBonus = dot', editPushedNodes = [] }

editPushDotTele edit node = 
    case editPushedNodes edit of
        (n0:ns)   -> 
            let dot = editDotTele edit
                dot' = dot { dotteleNode = n0, dotteleNode' = node }
                edit' = editModifySemiContent edit $ \cnt -> scontentPushDotTele cnt (scontentRoom cnt) dot'
            in  edit' { editDotTele = dot', editPushedNodes = [] }

        []        ->
            edit { editPushedNodes = editPushedNodes edit ++ [node] }

editPushDotFinish edit node = 
    let dot = editDotFinish edit
        dot' = dot { dotfinishNode = node }
        edit' = editModifySemiContent edit $ \cnt -> scontentPushDotFinish cnt (scontentRoom cnt) dot'
    in  edit' { editDotFinish = dot', editPushedNodes = [] }

editPushWall edit node = 
    case editPushedNodes edit of
        (n0:n1:ns)   -> 
            let wall = editWall edit
                wall' = wall 
                        {
                            wallNode = n0,
                            wallX = nodeDiff n0 n1,
                            wallY = nodeDiff n0 node
                        } 
                edit' = editModifySemiContent edit $ \cnt -> scontentPushWall cnt (scontentRoom cnt) wall'
            in  edit' { editWall =  wall', editPushedNodes = [] }

        _            ->
            edit { editPushedNodes = editPushedNodes edit ++ [node] }


        
