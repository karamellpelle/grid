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
module MEnv.System.GLFW
  (
    systemHandleFrontBegin,
    systemHandleFrontEnd,
    systemHandleBackBegin,
    systemHandleBackEnd,

  ) where

import MEnv


systemHandleFrontBegin :: a -> a -> MEnv res a
systemHandleFrontBegin a a' = 
    io $ putStrLn "GLFW.systemHandleFrontBegin" >> return a

systemHandleFrontEnd :: a -> a -> MEnv res a
systemHandleFrontEnd a a' = 
    io $ putStrLn "GLFW.systemHandleFrontEnd" >> return a


systemHandleBackBegin :: a -> a -> MEnv res a
systemHandleBackBegin a a' = 
    io $ putStrLn "GLFW.systemHandleBackBegin" >> return a


systemHandleBackEnd :: a -> a -> MEnv res a
systemHandleBackEnd a a' = 
    io $ putStrLn "GLFW.systemHandleBackEnd" >> return a






{-

data Friend 

data FriendData =
    meta :: XXX,
    content :: BS.ByteString


friendsEatData :: ([FriendData] -> [FriendData]) -> MEnv res ()


-}
