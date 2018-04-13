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
module Game.Modify
  (
    defaultModify,

    noControlModify,
    noBeginModify,
    noUpdateModify,

  ) where


import Game.MEnv


defaultModify :: (s -> a -> b -> MEnv' (s, a, b)) ->
                 (s -> a -> b -> MEnv' (s, a, b)) ->
                 (s -> a -> b -> MEnv' (s, a, b)) ->
                 s -> a -> b -> MEnv' (s, a, b)

defaultModify beginModify
              controlModify
              updateModify = \s a b -> do

    -- modify world at beginning of step (like clean up)
    (s', a', b') <- beginModify s a b

    -- modify world from controls (like input, network)
    (s'', a'', b'') <- controlModify s' a' b'

    -- modify world by updating it
    (s''', a''', b''') <- updateModify s'' a'' b''

    return (s''', a''', b''')



noControlModify :: s -> a -> b -> MEnv' (s, a, b)
noControlModify s a b =
    return (s, a, b)

noBeginModify :: s -> a -> b -> MEnv' (s, a, b)
noBeginModify s a b =
    return (s, a, b)

noUpdateModify :: s -> a -> b -> MEnv' (s, a, b)
noUpdateModify s a b =
    return (s, a, b)





