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
module Game.Run.Helpers.Message
  (
    charToSegments,

  ) where

import MyPrelude
import Game
import Game.Grid

import Data.Int




charToSegments :: Char -> [Segment]
charToSegments ' ' = mkSegs2 []
charToSegments '!' = mkSegs2 [ [(2, 0), (2, 1)], [(2, 2), (2, 5)] ]
charToSegments '"' = mkSegs2 [ [(2, 4), (2, 5)], [(3, 4), (3, 5) ] ]
charToSegments '#' = mkSegs2 [ [(1, 0), (1, 5)], [(2, 0), (2, 5)], [(0, 4), (3, 4)], [(0, 1), (3, 1)] ]
charToSegments '$' = mkSegs2 [ [(0, 1), (3, 1), (3, 3), (0, 3), (0, 4), (3, 4)], 
                               [(1, 5), (1, 0)], [(2, 5), (2, 0)] ]
charToSegments '%' = mkSegs2 [ [(1, 0), (1, 2), (2, 2), (2, 4), (3, 4), (4, 6)], 
                               [(1, 5), (1, 4)], [(3, 2), (3, 1) ] ]
charToSegments '&' = mkSegs2 [ [(3, 0), (3, 2), (1, 2), (1, 5), (3, 5), (3, 3), (0, 3), (0, 0), (4, 0)] ]
charToSegments '\'' = mkSegs2 [ [(2, 4), (2, 5)] ]
charToSegments '(' = mkSegs2 [ [(3, 5), (1, 5), (1, 4), (0, 4), (0, 1), (1, 1), (1, 0), (3, 0)] ]
charToSegments ')' = mkSegs2 [ [(0, 0), (2, 0), (2, 1), (3, 1), (3, 4), (2, 4), (2, 5), (0, 5)] ]
charToSegments '*' = mkSegs2 [ [(2, 4), (3, 4)], [(3, 3), (3, 2)], [(2, 2), (1, 2)], [(1, 3), (1, 4)],
                               [(2, 5), (2, 1)], [(0, 3), (4, 3)] ]
charToSegments '+' = mkSegs2 [ [(2, 2), (2, 4)], [(1, 3), (3, 3)]  ]
charToSegments ',' = mkSegs2 [ [(1, 0), (2, 0), (2, 1)] ]
charToSegments '-' = mkSegs2 [ [(1, 3), (3, 3)] ]
charToSegments '.' = mkSegs2 [ [(2, 0), (2, 1)] ]
charToSegments '/' = mkSegs2 [ [(1, 0), (1, 2), (2, 2), (2, 4), (3, 4), (3, 6)] ]
charToSegments '0' = mkSegs2 [ [(0, 0), (3, 0), (3, 5), (0, 5), (0, 0)] ]
charToSegments '1' = mkSegs2 [ [(2, 0), (2, 5)], [(1, 4), (2, 4)] ]
charToSegments '2' = mkSegs2 [ [(0, 5), (3, 5), (3, 3), (0, 3), (0, 0), (3, 0)] ]
charToSegments '3' = mkSegs2 [ [(0, 0), (3, 0), (3, 5), (0, 5)], [(0, 3), (3, 3)] ]
charToSegments '4' = mkSegs2 [ [(2, 0), (2, 5)], [(0, 5), (0, 3), (3, 3)] ]
charToSegments '5' = mkSegs2 [ [(0, 0), (3, 0), (3, 3), (0, 3), (0, 5), (3, 5)] ]
charToSegments '6' = mkSegs2 [ [(0, 3), (3, 3), (3, 0), (0, 0), (0, 5), (3, 5)] ]
charToSegments '7' = mkSegs2 [ [(2, 0), (2, 5), (0, 5)], [(1, 3), (3, 3)] ]
charToSegments '8' = mkSegs2 [ [(0, 0), (3, 0), (3, 5), (0, 5), (0, 0)], [(0, 3), (3, 3)] ]
charToSegments '9' = mkSegs2 [ [(0, 0), (3, 0), (3, 5), (0, 5), (0, 3), (3, 3)] ]
charToSegments ':' = mkSegs2 [ [(2, 0), (2, 1)], [(2, 3), (2, 4)] ]
charToSegments ';' = mkSegs2 [ [(1, 0), (2, 0), (2, 1)], [(2, 3), (2, 4)] ]
charToSegments '<' = mkSegs2 [ [(3, 4), (2, 4), (2, 3), (1, 3), (1, 2), (2, 2), (2, 1), (3, 1)] ]
charToSegments '=' = mkSegs2 [ [(1, 2), (3, 2)], [(1, 4), (3, 4)] ]
charToSegments '>' = mkSegs2 [ [(0, 1), (1, 1), (1, 2), (2, 2), (2, 3), (1, 3), (1, 4), (0, 4)] ]
charToSegments '?' = mkSegs2 [ [(1, 5), (3, 5), (3, 3), (2, 3), (2, 2)], [(2, 1), (2, 0)] ]
charToSegments '@' = mkSegs2 [ [(1, 4), (2, 4), (2, 2), (1, 2), (1, 3), (2, 3)], 
                               [(2, 2), (3, 2), (3, 5), (0, 5), (0, 1), (4, 1) ] ]
charToSegments 'A' = mkSegs2 [ [(0, 0), (0, 5), (3, 5), (3, 0)], [(0, 4), (3, 4) ] ]
charToSegments 'B' = mkSegs2 [ [(2, 3), (2, 5), (0, 5), (0, 3)], [(3, 0), (3, 3), (0, 3), (0, 0), (3, 0)] ] 
charToSegments 'C' = mkSegs2 [ [(3, 5), (0, 5), (0, 0), (3, 0)] ]
charToSegments 'D' = mkSegs2 [ [(0, 0), (3, 0), (3, 4), (2, 4), (2, 5), (0, 5), (0, 0)] ]
charToSegments 'E' = mkSegs2 [ [(0, 4), (2, 4)], [(2, 5), (0, 5), (0, 0), (3, 0)] ]
charToSegments 'F' = mkSegs2 [ [(0, 0), (0, 5), (2, 5)], [(0, 3), (3, 3) ] ]
charToSegments 'G' = mkSegs2 [ [(1, 2), (1, 3), (3, 3), (3, 0), (0, 0), (0, 5), (2, 5)] ]
charToSegments 'H' = mkSegs2 [ [(0, 0), (0, 5)], [(0, 4), (3, 4)], [(3, 5), (3, 0)] ]
charToSegments 'I' = mkSegs2 [ [(2, 0), (2, 5)] ]
charToSegments 'J' = mkSegs2 [ [(0, 0), (3, 0), (3, 5)] ]
charToSegments 'K' = mkSegs2 [ [(0, 0), (0, 5)], [(0, 3), (2, 3), (2, 5)], [(2, 3), (3, 3), (3, 0)] ]
charToSegments 'L' = mkSegs2 [ [(0, 5), (0, 0), (3, 0)] ]
charToSegments 'M' = mkSegs2 [ [(0, 0), (0, 5), (2, 5), (2, 0)], [(2, 5), (3, 5), (3, 0)] ]
charToSegments 'N' = mkSegs2 [ [(0, 0), (0, 5), (2, 5), (2, 0)] ]
charToSegments 'O' = mkSegs2 [ [(0, 0), (3, 0), (3, 5), (0, 5), (0, 0)] ]
charToSegments 'P' = mkSegs2 [ [(0, 4), (2, 4), (2, 5), (0, 5), (0, 0)] ]
charToSegments 'Q' = mkSegs2 [ [(0, 0), (3, 0), (3, 5), (0, 5), (0, 0)], [(1, 1), (2, 1), (2, 0)]  ]
charToSegments 'R' = mkSegs2 [ [(0, 3), (2, 3), (2, 5), (0, 5), (0, 0)], [(2, 3), (3, 3), (3, 0)] ]
charToSegments 'S' = mkSegs2 [ [(0, 0), (3, 0), (3, 4), (0, 4), (0, 5), (3, 5)] ]
charToSegments 'T' = mkSegs2 [ [(1, 0), (1, 5)], [(0, 5), (3, 5)] ]
charToSegments 'U' = mkSegs2 [ [(0, 5), (0, 0), (3, 0), (3, 5)] ]
charToSegments 'V' = mkSegs2 [ [(1, 5), (1, 2), (2, 2), (2, 0), (3, 0), (3, 5)] ]
charToSegments 'W' = mkSegs2 [ [(0, 5), (0, 2), (1, 2), (1, 0), (2, 0), (2, 5)], [(2, 0), (3, 0), (3, 5)] ]
charToSegments 'X' = mkSegs2 [ [(1, 0), (1, 5)], [(0, 5), (0, 3), (3, 3), (3, 0)] ]
charToSegments 'Y' = mkSegs2 [ [(0, 5), (0, 4), (3, 4)], [(3, 5), (3, 0)] ]
charToSegments 'Z' = mkSegs2 [ [(0, 5), (3, 5), (3, 4), (1, 4), (1, 3), (0, 3), (0, 0), (3, 0) ] ]
--charToSegments 'Æ' = mkSegs2 [ [(0, 0), (0, 5), (2, 5), (2, 0)], [(2, 5), (3, 5)],
--                               [(0, 4), (3, 4), [(2, 0), (3, 0)] ]
--charToSegments 'Ø' = mkSegs2 [ [(0, 0), (3, 0), (3, 5), (0, 5), (0, 0)], 
--                               [(0, 1), (1, 1), (1, 2), (2, 2), (2, 3), (3, 3) ] ]
--charToSegments 'Å' = mkSegs2 [ [(0, 0), (0, 4), (3, 4), (3, 0)], 
--                               [(0, 3), (3, 3)], [(2, 5), (2, 6), (1, 6), (1, 5)] ]
charToSegments '[' = mkSegs2 [ [(3, 5), (1, 5), (1, 0), (3, 0)] ]
charToSegments '\\' = mkSegs2 [ [(1, 6), (1, 4), (2, 4), (2, 2), (3, 2), (3, 0)] ]
charToSegments ']' = mkSegs2 [ [(1, 0), (3, 0), (3, 5), (1, 5)] ]
charToSegments '^' = mkSegs2 [ [(0, 4), (0, 5), (1, 5), (1, 6), (2, 6), (2, 5), (3, 5), (3, 4)] ]
charToSegments '_' = mkSegs2 [ [(0, 0), (4, 0)] ]
charToSegments '`' = mkSegs2 [ [(1, 5), (2, 5), (2, 4)] ]
charToSegments 'a' = mkSegs2 [ [(1, 3), (3, 3), (3, 0), (0, 0), (0, 2), (3, 2)] ]
charToSegments 'b' = mkSegs2 [ [(1, 3), (3, 3), (3, 0), (1, 0), (1, 5)] ]
charToSegments 'c' = mkSegs2 [ [(3, 3), (1, 3), (1, 0), (4, 0)] ]
charToSegments 'd' = mkSegs2 [ [(3, 5), (3, 0), (1, 0), (1, 3), (3, 3)] ]
charToSegments 'e' = mkSegs2 [ [(1, 2), (3, 2), (3, 3), (1, 3), (1, 0), (4, 0)] ]
charToSegments 'f' = mkSegs2 [ [(1, 0), (1, 5), (2, 5)], [(0, 2), (2, 2)] ]
charToSegments 'g' = mkSegs2 [ [(0, 1), (0, 0), (3, 0), (3, 3), (1, 3), (1, 2), (3, 2)] ]
charToSegments 'h' = mkSegs2 [ [(1, 0), (1, 5)], [(1, 3), (3, 3), (3, 0)] ]
charToSegments 'i' = mkSegs2 [ [(2, 0), (2, 3)], [(2, 4), (2, 5)] ]
charToSegments 'j' = mkSegs2 [ [(0, 1), (0, 0), (3, 0), (3, 3)], [(3, 4), (3, 5)] ]
charToSegments 'k' = mkSegs2 [ [(1, 0), (1, 5)], [(1, 2), (2, 2), (2, 3)], [(2, 2), (3, 2), (3, 0)] ]
charToSegments 'l' = mkSegs2 [ [(2, 0), (2, 5)] ]
charToSegments 'm' = mkSegs2 [ [(1, 0), (1, 3), (2, 3), (2, 0)], [(2, 3), (3, 3), (3, 0)] ]
charToSegments 'n' = mkSegs2 [ [(1, 0), (1, 3), (2, 3), (2, 0)] ]
charToSegments 'o' = mkSegs2 [ [(1, 0), (3, 0), (3, 3), (1, 3), (1, 0)] ]
charToSegments 'p' = mkSegs2 [ [(1, 2), (3, 2), (3, 3), (1, 3), (1, 0)] ]
charToSegments 'q' = mkSegs2 [ [(3, 0), (3, 3), (1, 3), (1, 2), (3, 2)], [(2, 1), (4, 1)] ]
charToSegments 'r' = mkSegs2 [ [(1, 0), (1, 3), (3, 3)] ]
charToSegments 's' = mkSegs2 [ [(0, 0), (3, 0), (3, 2), (1, 2), (1, 3), (3, 3)] ]
charToSegments 't' = mkSegs2 [ [(1, 3), (3, 3)], [(2, 4), (2, 0), (4, 0)] ]
charToSegments 'u' = mkSegs2 [ [(1, 3), (1, 0), (3, 0), (3, 3)] ]
charToSegments 'v' = mkSegs2 [ [(1, 3), (1, 1), (2, 1), (2, 0), (3, 0), (3, 3)] ]
charToSegments 'w' = mkSegs2 [ [(0, 3), (0, 1), (1, 1), (1, 0), (2, 0), (2, 3)], [(2, 0), (3, 0), (3, 3)] ]
charToSegments 'x' = mkSegs2 [ [(1, 3), (1, 2), (3, 2), (3, 0)], [(2, 0), (2, 3)] ]
charToSegments 'y' = mkSegs2 [ [(1, 3), (1, 2), (3, 2)], [(0, 0), (3, 0), (3, 3)] ]
charToSegments 'z' = mkSegs2 [ [(0, 3), (2, 3), (2, 2), (1, 2), (1, 1), (0, 1), (0, 0), (3, 0)] ]
--charToSegments 'æ' = mkSegs2 [ [(1, 3), (3, 3), (3, 2), (0, 2), (0, 0), (2, 0)], [(2, 3), (2, 0), (3, 0)] ]
--charToSegments 'ø' = mkSegs2 [ [(0, 0), (3, 0), (3, 3), (0, 3), (0, 0)], 
--                               [(1, 0), (1, 1), (2, 1), (2, 2), (3, 2)] ]
--charToSegments 'å' = mkSegs2 [ [(1, 3), (3, 3), (3, 0), (0, 0), (0, 2), (3, 2)], 
--                               [(2, 4), (3, 4), (3, 5), (2, 5), (2, 4)] ]
charToSegments '{' = mkSegs2 [ [(3, 5), (2, 5), (2, 3), (1, 3), (1, 2), (2, 2), (2, 0), (3, 0)] ]
charToSegments '|' = mkSegs2 [ [(2, 0), (2, 6)] ]
charToSegments '}' = mkSegs2 [ [(1, 0), (2, 0), (2, 2), (3, 2), (3, 3), (2, 3), (2, 5), (1, 5)] ]
charToSegments '~' = mkSegs2 [ [(1, 2), (1, 3), (2, 3), (2, 2), (3, 2), (3, 3)] ]
-- special character '\0': wait
charToSegments '\0' = replicate (fI valueRunMessageSegsPerWait) dummySegment
-- special character '\t': space and eat
charToSegments '\t' = replicate (fI valueRunMessageCharWth) dummySegment
-- other characters are empty (like space)
charToSegments _   = mkSegs2 []



--------------------------------------------------------------------------------
--  

mkSegs3 :: [[(Int16, Int16, Int16)]] -> [Segment]
mkSegs3 pss =
    helper $ setHanded pss
    where
        setHanded = 
            id --map (map (\(x, y, z) -> (x, y, z)))           -- right handed
            --map (map (\(x, y, z) -> (x, y, negate z)))    -- left handed
        helper (ps:pss) =
            helper' ps ++ helper pss
        helper [] =
            []
        -- assuming p0 != p1, p0 and p1 on same axis 
        helper' (p0:p1:ps) =         
            let (dx, dy, dz) = diff p0 p1
                n = diffN dx dy dz 
                dxyz = (div dx n, div dy n, div dz n)
            in  helper'' p0 dxyz (findTurn dxyz) n ++ helper' (p1:ps)
        helper' _ = 
            []
        helper'' (x, y, z) dxyz@(dx, dy, dz) turn n =
            if n == 0 then []
                      else Segment (Node x y z) turn : 
                           helper'' (x + dx, y + dy, z + dz) dxyz turn (pred n)

        diffN dx dy dz =
            let a = abs dx
                b = abs dy
                c = abs dz
            in if a <= b then if b <= c then c
                                        else b
                         else if a <= c then c
                                        else a

        diff (x, y, z) (x', y', z') =
            (x' - x, y' - y, z' - z)

        findTurn dxyz =
            case dxyz of
                (0, 0, -1)    -> leftTurn
                (0, 0, 1)     -> rightTurn
                (0, -1, 0)    -> downTurn
                (0, 1, 0)     -> upTurn
                (-1, 0, 0)    -> backTurn
                (1, 0, 0)     -> straightTurn
                _             -> straightTurn


mkSegs2 :: [[(Int16, Int16)]] -> [Segment]
mkSegs2 =
    mkSegs3 . map (map (\(x, y) -> (x, y, 0)))





