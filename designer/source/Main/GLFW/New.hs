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
module Main.GLFW.New
  (
    mainNew,

  ) where

import MyPrelude
import File
import LoadM

import Game
import Game.GameData
import LevelTools.EditWorld
import LevelTools.Helpers
import LevelTools.EditWorld.Make
import LevelTools.Iteration
import LevelTools.Make
import LevelTools.File


import OpenGL
import OpenGL.Helpers
import Data.Maybe

import Graphics.UI.GLFW as GLFW
import System.IO


mainNew = do
    
    let init = error "no EnvInit"
    
    -- run MEnv!
    (edit, _) <- runGLFWMEnv init loadGameData unloadGameData 
                    begin iterate ()

    case findInvalidObject edit of
        Nothing                   -> return ()
        Just (room, node, str)  -> do
            putStrLn $ "warning        : invalid at RoomIx: " ++ show room ++ ", " ++ show node ++
                       " (" ++ str ++ ")"

    -- prompt for names and write to file
    putStr $    "name           : "
    hFlush stdout

    name <- getLine
    let edit' = editModifyLevel edit $ \level -> level { levelName = name }
    path <- case name of 
            []    -> findSimilarFileName "_" ".ldef"
            name  -> findSimilarFileName (basenameFromString name) ".ldef"

    writeEditAsLevel edit' path
    putStrLn $  "written to file: " ++ path

    where
      begin _ = do
          io $ do
              -- OpenGL --
              -- see invariants.txt for GL state
              glClearColor 0 0 0 1

              glEnable gl_BLEND                              
              glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA 
              glDepthFunc gl_LEQUAL
              glEnable gl_MULTISAMPLE
         
          -- if first run, create folders and files for dynamic data
          createDynamicData

          edit <- makeEditWorldNew 

          -- play game
          -- return (edit, (), [])
          return (edit, (), [iterationEdit])

      iterate (a, b, stack) = do
          runABStack a b stack


--------------------------------------------------------------------------------
--  

runABStack :: a -> b -> IterationStack a b -> MEnv' (a, b)
runABStack a b (i:is) = do
    screenBegin
    tickBegin
    keysBegin
    
    (a', b', top) <- (iteration i) a b
    
    keysEnd
    tickEnd
    screenEnd
{-
-- tmp
    state <- io $ GLFW.getKey (GLFW.SpecialKey GLFW.ESC)
    case state of
      GLFW.Release    -> runABStack a' b' (top ++ is)
      GLFW.Press      -> return (a', b')
--
-}
    runABStack a' b' (top ++ is)


runABStack a b [] = do
    return (a, b)

