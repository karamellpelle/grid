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
{-# LANGUAGE ForeignFunctionInterface #-}
module MEnv.Players.GLFW
  (
    Player (..),

    playersAuthenticateLocalPlayer,
    playersHandleLocalPlayer,
    playersSendAchievement,
    playersSendScore,

  ) where

import MyPrelude
import Foreign
import Foreign.C
import MEnv


data Player =
    Player
    {
        playerID :: String,
        playerAlias :: String
    }


--------------------------------------------------------------------------------
--  

-- | try to play as a local player. the local player is handled with 
--   playersHandleLocalPlayer, and may be called multiple times with
--   different players during the lifetime of calling program
playersAuthenticateLocalPlayer :: MEnv res ()
playersAuthenticateLocalPlayer = io $ 
    putStrLn "GLFW.playersAuthenticateLocalPlayer"




-- | play with given local player.
--
playersHandleLocalPlayer :: a -> (Player -> a) -> MEnv res a
playersHandleLocalPlayer a f = io $ do
    putStrLn "GLFW.playersHandleLocalPlayer"




playersLocalPlayer :: MEnv res (Maybe Player)
playersLocalPlayer = io $ do
    putStrLn "GLFW.playersLocalPlayer"


-- | send value 'alpha' (in [0, 1]) to achievement 'ach', for local player
playersSendAchievement :: String -> Float -> MEnv res ()
playersSendAchievement ach alpha = io $ 
    putStrLn "GLFW.playersSendAchievement"



-- | send score value 'score' of category 'cat', for local player
playersSendScore :: String -> Int64 -> MEnv res ()
playersSendScore cat score = io $ 
    putStrLn "GLFW.playersSendScore"


