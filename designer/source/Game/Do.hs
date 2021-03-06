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
module Game.Do
  (
    defaultDo,
    noBreakModify,
    noDefaultModify,


  ) where


import MyPrelude
import MEnv.Tick
import Game.MEnv
import Game.World



defaultDo :: World a e =>
        (s -> a -> b -> MEnv' (s, a, b)) ->
        (MEnv' TickT, TickT -> MEnv' (), TickT, TickT) ->
        (TickT -> s -> a -> b -> MEnv' (s, a, b)) ->
        (s -> a -> b -> MEnv' (Maybe (s, a, b))) ->
        (s -> a -> b -> MEnv' (s, a, b)) -> 
        s -> a -> b -> MEnv' (s, a, b)
defaultDo modify
     (getTick, setTick, dtUnit, maxElaps)
     stepDT
     breakModify
     defaultModify = \s a b -> do

        -- modify world begin
        (s', a', b') <- modify s a b

        -- ignore too long elaps
        tick <- getTick
        when ( worldTick a' + maxElaps <= tick ) $ do
            setTick ( worldTick a' + maxElaps )
        tick <- getTick

        -- step physics in 'dtUnit' portions
        helper tick s' a' b'
        where
          helper tick s a b =
            if worldTick a + dtUnit <= tick

              -- take a 'dtUnit'-step of physical objects
              then do
                (s', a', b') <- stepDT dtUnit s a b
                maybeSAB <- breakModify s' a' b'
                case maybeSAB of
                    Nothing               -> helper tick s' a' b'
                    -- modify world end
                    Just (s'', a'', b'')  -> return (s'', a'', b'')

              -- modify world end
              else do
                defaultModify s a b



noBreakModify :: s -> a -> b -> MEnv' (Maybe (s, a, b))
noBreakModify s a b =
    return Nothing


noDefaultModify :: s -> a -> b -> MEnv' (s, a, b)
noDefaultModify s a b =
    return (s, a, b)


