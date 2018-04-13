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
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Game.Helpers.World
  (
--------------------------------------------------------------------------------
--  fixme: clean up all these

    handleAllEvents,
    handleEventsBreak,
    handleOneEvent,
    breakEvents,
    continueEvents,

    handleAllEventsM,
    handleAllEventsM_,
    handleEventsBreakM,
    breakEventsM,
    continueEventsM,
    handleOneEventM,
    handleEventM,
    nextEventM,

  ) where


import Game.World


handleAllEvents :: World w e => w -> a -> (a -> e -> a) -> a
handleAllEvents w a f = 
    helper a (worldAllEvents w)
    where
      helper a (e:es) =
          helper (f a e) es
      helper a [] =
          a

handleEventsBreak :: World w e => w -> a -> (a -> e -> (Bool, a)) -> a
handleEventsBreak w a f =
    helper a (worldAllEvents w)
    where
      helper a (e:es) =
        case f a e of
            (False, a') -> helper a' es
            (True, a')  -> a'
      helper a [] =
          a

breakEvents :: a -> (Bool, a)
breakEvents a =
    (True, a)

continueEvents :: a -> (Bool, a)
continueEvents a =
    (False, a)


handleOneEvent :: World w e => w -> a -> (e -> Maybe a) -> a
handleOneEvent w end f =
    helper (worldAllEvents w)
    where
        helper (e:es) =
            case f e of
                Nothing   -> helper es
                Just a    -> a
        helper [] =
            end


handleAllEventsM :: (Monad m, World w e) => w -> a -> (a -> e -> m a) -> m a
handleAllEventsM w a f = 
    helper a (worldAllEvents w)
    where
      helper a (e:es) = do
          a' <- f a e
          helper a' es
      helper a [] =
          return a


handleAllEventsM_ :: (Monad m, World w e) => w -> (e -> m ()) -> m ()
handleAllEventsM_ w f =
    mapM_ f (worldAllEvents w)


handleEventsBreakM :: (Monad m, World w e) => w -> a -> (a -> e -> m (Bool, a)) -> m a
handleEventsBreakM w a f =
    helper a (worldAllEvents w)
    where
      helper a (e:es) = do
          (break, a') <- f a e
          case break of
              False   -> helper a' es
              True    -> return a'
      helper a [] = 
          return a


breakEventsM :: Monad m => a -> m (Bool, a)
breakEventsM a = do
    return (True, a)


continueEventsM :: Monad m => a -> m (Bool, a)
continueEventsM a = do
    return (False, a)


handleOneEventM :: (Monad m, World w e) => w -> m a -> (e -> m (Maybe a)) -> m a
handleOneEventM w end f =
    helper (worldAllEvents w)
    where
        helper (e:es) =
            f e >>= \maybeA -> case maybeA of
                Nothing   -> helper es
                Just a    -> return a
        helper [] =
            end


handleEventM :: Monad m => m a -> m (Maybe a)
handleEventM ma =
    ma >>= \a -> return $ Just a


nextEventM :: Monad m => m (Maybe a)
nextEventM =
    return $ Nothing
