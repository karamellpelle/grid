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
module MEnv.Players.IOS
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

foreign import ccall unsafe "&ios_players_new_local_player" 
    ios_players_new_local_player :: Ptr CUInt

foreign import ccall unsafe "&ios_players_local_player_id_len" 
    ios_players_local_player_id_len :: Ptr CUInt

foreign import ccall unsafe "&ios_players_local_player_alias_len" 
    ios_players_local_player_alias_len :: Ptr CUInt

foreign import ccall unsafe "&ios_players_local_player_id" 
    ios_players_local_player_id :: (Ptr (Ptr CChar))

foreign import ccall unsafe "&ios_players_local_player_alias" 
    ios_players_local_player_alias :: (Ptr (Ptr CChar))


-- | try to play as a local player. the local player is handled with 
--   playersHandleLocalPlayer, and may be called multiple times with
--   different players during the lifetime of calling program
playersAuthenticateLocalPlayer :: MEnv res ()
playersAuthenticateLocalPlayer = io $ 
    ios_playersAuthenticateLocalPlayer

foreign import ccall unsafe "ios_playersAuthenticateLocalPlayer" 
    ios_playersAuthenticateLocalPlayer :: IO ()



-- | play with given local player.
--
--          "Never make assumptions about the format or length of player identifier 
--          strings" - iOS Documentation
--
--          :)
playersHandleLocalPlayer :: a -> (Player -> a) -> MEnv res a
playersHandleLocalPlayer a f = io $ do
    peek ios_players_new_local_player >>= \v -> case v of
        0     -> return a
        _     -> do
            id <- peek ios_players_local_player_id
            alias <- peek ios_players_local_player_alias
            idLen <- peek ios_players_local_player_id_len
            aliasLen <- peek ios_players_local_player_alias_len

            id' <- peekCStringLen (id, fI idLen)
            alias' <- peekCStringLen (alias, fI aliasLen)
            poke ios_players_new_local_player 0
            return $ f (Player id' alias')



playersLocalPlayer :: MEnv res (Maybe Player)
playersLocalPlayer = io $ do
    peek ios_players_local_player_id >>= \id -> if id == nullPtr 
        then return Nothing
        else do     
            --id <- peek ios_players_local_player_id
            alias <- peek ios_players_local_player_alias
            idLen <- peek ios_players_local_player_id_len
            aliasLen <- peek ios_players_local_player_alias_len

            id' <- peekCStringLen (id, fI idLen)
            alias' <- peekCStringLen (alias, fI aliasLen)
            return (Just $ Player id' alias')



-- | send value 'alpha' (in [0, 1]) to achievement 'ach', for local player
playersSendAchievement :: String -> Float -> MEnv res ()
playersSendAchievement ach alpha = io $ 
    withCString ach $ \ptrAch -> 
        ios_playersSendAchievement ptrAch (rTF alpha)

foreign import ccall unsafe "ios_playersSendAchievement" 
    ios_playersSendAchievement :: CString -> CFloat -> IO ()




-- | send score value 'score' of category 'cat', for local player
playersSendScore :: String -> Int64 -> MEnv res ()
playersSendScore cat score = io $ 
    withCString cat $ \ptrCat -> 
        ios_playersSendScore ptrCat (fI score)

-- from types.h : typedef long long int64_t; 
foreign import ccall unsafe "ios_playersSendScore" 
    ios_playersSendScore :: CString -> CLLong -> IO () 


