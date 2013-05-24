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
module Game.Step
  (
    defaultStep,
    noDo,

  ) where


import Game.Iteration
import Game.MEnv


-- | defaultStep is 'do-think' instead of 'think-do'
defaultStep :: (s -> a -> b -> MEnv' (s, a, b)) -> 
               (s -> a -> b -> MEnv' (a, b, IterationStack a b)) -> 
               s -> a -> b -> MEnv' (a, b, IterationStack a b)
defaultStep doWorld
            thinkWorld = \s a b -> do
    -- do
    (s', a', b') <- doWorld s a b

    -- think
    thinkWorld s' a' b' 


noDo :: s -> a -> b -> MEnv' (s, a, b)
noDo s a b =
    return (s, a, b)


