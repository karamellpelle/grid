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
module Game.Grid.Output.Fancy.SoundGrid
  (
    soundPathNewSegment,

  ) where

import MyPrelude
import Game
import Game.Grid

import OpenAL
import OpenAL.Helpers


soundPathNewSegment :: SoundPath -> Segment -> Float -> IO ()
soundPathNewSegment sound (Segment (Node x y z) turn) pitch = do
    let src = soundPathSrc sound
    
    -- pitch
    alSourcef src al_PITCH $ rTF pitch

    -- position
    alSource3f src al_POSITION (fI x) (fI y) (fI z)

    -- direction
    --let Dir a0 a1 a2 = direction turn
    --alSource3f src al_DIRECTION (fI a0) (fI a1) (fI a2)

    -- velocity?

    alSourcePlay src
