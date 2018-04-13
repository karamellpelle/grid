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
module LevelTools.Helpers where


import MyPrelude
import LevelTools.EditWorld
import Game.Grid.Helpers


findInvalidObject :: EditWorld -> Maybe (RoomIx, Node, String)
findInvalidObject edit = 
    let ixs = map (\sr -> sroomRoomIx sr) $ scontentRooms $ editSemiContent edit
        dotplains = concatMap (\sr -> map (\dot -> (sroomRoomIx sr, dot)) (sroomDotPlain sr)) $ scontentRooms $ editSemiContent edit

    in  helper ixs dotplains
    where
      helper :: [RoomIx] -> [(RoomIx, DotPlain)] -> Maybe (RoomIx, Node, String)
      helper ixs ((room, dot):rds) =
          if dotplainRoom dot `elem` ixs 
            then helper ixs rds
            else Just (room, dotplainNode dot, "no such RoomIx: " ++ show (dotplainRoom dot))
      helper ixs [] = 
          Nothing
         

editModifyCamera edit f =
    editModifyGrid edit $ \grid -> gridModifyCamera grid f

editModifyGrid edit f =
    edit { editGrid = f (editGrid edit) }


editModifyLevel edit f = 
    edit { editLevel = f $ editLevel edit }

editModifyCameraNode edit f =
    edit { editCameraNode = f $ editCameraNode edit }

editModifyNode edit f =
    edit { editNode = f $ editNode edit }

editModifySemiContent edit f =
    edit { editSemiContent = f $ editSemiContent edit }


editModifyDotPlain edit f =
    edit { editDotPlain = f $ editDotPlain edit }

editModifyDotBonus edit f =
    edit { editDotBonus = f $ editDotBonus edit }

editModifyDotTele edit f =
    edit { editDotTele = f $ editDotTele edit }

editModifyDotFinish edit f =
    edit { editDotFinish = f $ editDotFinish edit }

editModifyWall edit f =
    edit { editWall = f $ editWall edit }


scontentPushDotPlain cnt room dot = 
    cnt { scontentRooms = helper (scontentRooms cnt) room dot }
    where
      helper (r:rs) ix dot =
          if sroomRoomIx r == ix then let r' = r { sroomDotPlain = sroomDotPlain r ++ [dot] }
                                      in  r' : rs
                                 else r : helper rs ix dot
      helper [] ix dot =
          [makeSemiRoom ix [] [dot] [] [] []]

scontentPushDotBonus cnt room dot = 
    cnt { scontentRooms = helper (scontentRooms cnt) room dot }
    where
      helper (r:rs) ix dot =
          if sroomRoomIx r == ix then let r' = r { sroomDotBonus = sroomDotBonus r ++ [dot] }
                                      in  r' : rs
                                 else r : helper rs ix dot
      helper [] ix dot =
          [makeSemiRoom ix [] [] [dot] [] []]

scontentPushDotTele cnt room dot = 
    cnt { scontentRooms = helper (scontentRooms cnt) room dot }
    where
      helper (r:rs) ix dot =
          if sroomRoomIx r == ix then let r' = r { sroomDotTele = sroomDotTele r ++ [dot] }
                                      in  r' : rs
                                 else r : helper rs ix dot
      helper [] ix dot =
          [makeSemiRoom ix [] [] [] [dot] []]

scontentPushDotFinish cnt room dot = 
    cnt { scontentRooms = helper (scontentRooms cnt) room dot }
    where
      helper (r:rs) ix dot =
          if sroomRoomIx r == ix then let r' = r { sroomDotFinish = sroomDotFinish r ++ [dot] }
                                      in  r' : rs
                                 else r : helper rs ix dot
      helper [] ix dot =
          [makeSemiRoom ix [] [] [] [] [dot]]

scontentPushWall cnt room wall = 
    cnt { scontentRooms = helper (scontentRooms cnt) room wall }
    where
      helper (r:rs) ix wall =
          if sroomRoomIx r == ix then let r' = r { sroomWall = sroomWall r ++ [wall] }
                                      in  r' : rs
                                 else r : helper rs ix wall
      helper [] ix wall =
          [makeSemiRoom ix [wall] [] [] [] []]


--------------------------------------------------------------------------------
--  editObjects

editChangeObject edit node =
    let (edit', mWall, mDotPlain, mDotBonus, mDotTele, mDotFinish) = 
            modSemiContent edit $ \cnt -> modRoom cnt (scontentRoom cnt) $ \room -> 
                helper0 room node 
    in  edit'
        {
            editWall = maybe' (editWall edit) mWall,
            editDotPlain = maybe' (editDotPlain edit) mDotPlain,
            editDotBonus = maybe' (editDotBonus edit) mDotBonus,
            editDotTele = maybe' (editDotTele edit) mDotTele,
            editDotFinish = maybe' (editDotFinish edit) mDotFinish
        }
    
    where
          
      maybe' a = \maybeA -> case maybeA of
          Nothing   -> a
          Just a'   -> a'

      modSemiContent edit f =
          let (cnt', m0, m1, m2, m3, m4) = f (editSemiContent edit)
          in  (edit { editSemiContent = cnt' }, m0, m1, m2, m3, m4)

      modRoom cnt ix f = 
          let (rooms', m0, m1, m2, m3, m4) = helper (scontentRooms cnt) ix f 
          in  (cnt { scontentRooms = rooms' }, m0, m1, m2, m3, m4)
          where
            helper (r:rs) ix f = 
                if sroomRoomIx r == ix then let (r', m0, m1, m2, m3, m4) = f r
                                            in  (r':rs, m0, m1, m2, m3, m4)
                                       else  helper rs ix f
            helper [] ix f =
                ([], Nothing, Nothing, Nothing, Nothing, Nothing)


      helper0 room node =
          case helper (sroomDotPlain room) of
              Just (d, ds)  -> (room { sroomDotPlain = ds }, Nothing, Just d, Nothing, Nothing, Nothing)
              Nothing       -> helper1 room node
          where
            helper (d:ds) = 
                if dotplainNode d == node then Just (d, ds)
                  else case helper ds of
                      Just (d', ds')  -> Just (d', d:ds')
                      Nothing         -> Nothing
            helper [] = 
                Nothing

      helper1 room node =
          case helper (sroomDotBonus room) of
              Just (d, ds)  -> (room { sroomDotBonus = ds }, Nothing, Nothing, Just d, Nothing, Nothing)
              Nothing       -> helper2 room node
          where
            helper (d:ds) = 
                if dotbonusNode d == node then Just (d, ds)
                  else case helper ds of
                      Just (d', ds')  -> Just (d', d:ds')
                      Nothing         -> Nothing
            helper [] = 
                Nothing

      helper2 room node =
          case helper (sroomDotTele room) of
              Just (d, ds)  -> (room { sroomDotTele= ds }, Nothing, Nothing, Nothing, Just d, Nothing)
              Nothing       -> helper3 room node
          where
            helper (d:ds) = 
                if dotteleNode d == node || dotteleNode' d == node then Just (d, ds)
                  else case helper ds of
                      Just (d', ds')  -> Just (d', d:ds')
                      Nothing         -> Nothing
            helper [] =
                Nothing

      helper3 room node =
          case helper (sroomDotFinish room) of
              Just (d, ds)  -> (room { sroomDotFinish = ds }, Nothing, Nothing, Nothing, Nothing, Just d)
              Nothing       -> helper4 room node
          where
            helper (d:ds) = 
                if dotfinishNode d == node then Just (d, ds)
                  else case helper ds of
                      Just (d', ds')  -> Just (d', d:ds')
                      Nothing         -> Nothing
            helper [] =
                Nothing

      helper4 room node =
          case helper (sroomWall room) of
              Just (d, ds)  -> (room { sroomWall = ds }, Just d, Nothing, Nothing, Nothing, Nothing)
              Nothing       -> (room, Nothing, Nothing, Nothing, Nothing, Nothing) 
          where
            helper (d:ds) = 
                if isCol d node then Just (d, ds)
                  else case helper ds of
                      Just (d', ds')  -> Just (d', d:ds')
                      Nothing         -> Nothing
            helper [] =
                Nothing

            isCol wall node = 
              case nodeDiff (wallNode wall) node of
                p ->  let x = wallX wall 
                          y = wallY wall
                          n = nodeCross x y
                          ix = nodeInner p x
                          iy = nodeInner p y
                      in  nodeInner p n == 0 && 0 <= ix && ix <= nodeInner x x
                                             && 0 <= iy && iy <= nodeInner y y


editEraseObject edit node = 
    edit
