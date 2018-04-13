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
import MyPrelude
import LoadM
import OpenGL

import Game
import Game.Resource
import Game.Run.Iteration

import System.Environment
import LevelTools.EditWorld
import LevelTools.EditWorld.Constructs
import LevelTools.Iteration


-- tmp:
import Graphics.UI.GLFW as GLFW


main = do
    -- tmp: for ghci
    GLFW.terminate

    let init = EnvInit
              {
                  initScreen = ScreenInit,
                  initSound = SoundInit,
                  initTick = TickInit,
                  initKeys = KeysInit,
                  initForeign = ForeignInit,
                  initFriends = FriendsInit,
                  initPlatform = PlatformInit
              }
    -- settings for the environment are hardcoded in MEnv.Object.*, but the
    -- settings should insted be passed as an object to 'initEnvWithResource'
    runMEnv init withLoadedResource $ do
        
        -- set OpenGL
        io $ do
            glClearColor 0 0 0 1

            glEnable gl_BLEND                              
            glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA 
            glDepthFunc gl_LEQUAL
            glEnable gl_MULTISAMPLE
        
        -- key repeat
        tickSampleModifyUnit (const 0.3)

        args <- io $ getArgs
        case parseArgs args (Nothing, "out.lvl", Nothing) of
            (Nothing, _, Nothing)   -> do
                io $ putStrLn "usage: LevelTools [--in file] [--out file] [--name str], and (not infile => name)"
            
            (Nothing, outpath, Just name) -> do
                let edit = emptyEditWorld
                    edit' = edit { editLevelName = name,
                                   editPathOut = outpath }
                runIterationStack' edit' () [iterationEdit]
                return ()

            (Just inpath, outpath, Nothing) -> do
                edit <- loadEditWorld inpath
                let edit' = edit { editPathOut = outpath }
                runIterationStack' edit' () [iterationEdit]
                return ()

            (Just inpath, outpath, Just name) -> do
                edit <- loadEditWorld inpath
                let edit' = edit { editPathOut = outpath,
                                   editLevelName = name }
                runIterationStack' edit' () [iterationEdit]
                return ()




parseArgs (str:strs) (maybeInpath, outpath, maybeName) =
    case str of
        "--in"   -> case strs of
            []      -> error "expecting infile" 
            (s:ss)  -> parseArgs ss (Just s, outpath, maybeName)
        "--name" -> case strs of
            []      -> error "expecting name"
            (s:ss)  -> parseArgs ss (maybeInpath, outpath, Just s)
        "--out"   -> case strs of
            []      -> error "expecting outfile"
            (s:ss)  -> parseArgs ss (maybeInpath, s, maybeName)
        _         -> parseArgs strs (maybeInpath, outpath, maybeName)

parseArgs [] v =
    v
        
        --



-- | use stack to compute w a
runIterationStack' :: w -> a -> IterationStack w a -> MEnv' (w, a)
runIterationStack' w a (i:is) = do
    
    screenBegin
    tickBegin
    keysBegin
    
    (w', a', stack') <- (iteration i) w a
    
    keysEnd
    tickEnd
    screenEnd


    runIterationStack' w' a' (stack' ++ is)

runIterationStack' w a [] = do
    return (w, a)



