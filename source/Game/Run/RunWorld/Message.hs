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
module Game.Run.RunWorld.Message
  (
    Message (..),
    A,
    AS,
    ASS,
    ASSS,
    makeMessage,

  ) where


import MyPrelude
import Game
import Game.Grid


-- | current
type A =
    Segment

-- | segments describing character
type AS =
    [A]

-- | characters
type ASS =
    String

-- | strings. 
--   todo: [ MessageData ], where MessageData holds string, priority, color, etc...
type ASSS =
    [String]


data Message =
    Message
    {
        messageA :: !A,
        messageAS :: !AS,
        messageASS :: !ASS,
        messageASSS :: !ASSS,

        messageRef :: !Segment,
        messageRefPlus :: !Segment

    }


-- | make Message with Ref and RefPlus
makeMessage :: Segment -> Segment -> Message
makeMessage ref refPlus =
    Message
    {
        messageRef = ref,
        messageRefPlus = refPlus,
        messageA = mempty,
        messageAS = mempty,
        messageASS = mempty,
        messageASSS = mempty
    }

