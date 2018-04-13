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
module Game.Run.Output.Fancy
  (
    outputBegin,
    outputBegin',
    outputMain,
    outputMain',
    outputEnd,
    outputEnd',

    outputLevelMode,
    outputPuzzleMode,
    outputMemoryMode,
    outputForeign,
    outputKonami,

    outputAtFace',
    outputAtFace''',
    outputAToB,

  ) where


import MyPrelude
import Game

import Game.Grid.Helpers
import Game.Grid.Output
import Game.Grid.Output.Fancy
import Game.Run.RunWorld
import Game.Run.RunData
import Game.Run.Iteration.State
import Game.Run.Output.Fancy.Sound
import Game.Run.Output.Fancy.Screen
import Game.Run.Helpers
import OpenGL
import OpenGL.Helpers

import OpenAL
import OpenAL.Helpers


--------------------------------------------------------------------------------
-- Begin

outputBegin :: RunWorld -> MEnv' RunWorld
outputBegin = \run -> do
    
    -- fixme: is this necessary (cf. output of "Instruments")
    prewarmShaders 

    gamedata <- resourceGameData
    io $ do
      putStrLn "iterationBegin"
      outputSoundBegin gamedata run
      outputScreenBegin gamedata run
    
    return run


outputBegin' :: s -> RunWorld -> b -> MEnv' (s, RunWorld, b)
outputBegin' = \s run b -> do
    gamedata <- resourceGameData 
    let scene = runScene run
        proj2D = sceneProj2D scene
        proj3D = sceneProj3D scene
        modv3D = (cameraViewMat4 $ runCamera run) `mappend` 
                 (cameraModelMat4 $ runCamera run)

    io $ do
        outputSoundBegin' gamedata proj2D proj3D modv3D s run b
        outputScreenBegin' gamedata proj2D proj3D modv3D s run b

    return (s, run, b)



--------------------------------------------------------------------------------
--  Main

outputMain :: RunWorld -> MEnv' RunWorld
outputMain = \run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "iterationMain"
      outputSoundMain gamedata run
      outputScreenMain gamedata run

    return run


outputMain' :: s -> RunWorld -> b -> MEnv' (s, RunWorld, b)
outputMain' = \s run b -> do
    gamedata <- resourceGameData 
    let scene = runScene run
        proj2D = sceneProj2D scene
        proj3D = sceneProj3D scene
        modv3D = (cameraViewMat4 $ runCamera run) `mappend` 
                 (cameraModelMat4 $ runCamera run)

    io $ do
        outputSoundMain' gamedata proj2D proj3D modv3D s run b
        outputScreenMain' gamedata proj2D proj3D modv3D s run b

    return (s, run, b)


--------------------------------------------------------------------------------
--  End

outputEnd :: RunWorld -> MEnv' RunWorld
outputEnd = \run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "iterationEnd"
      outputSoundEnd gamedata run
      outputScreenEnd gamedata run
    
    return run


outputEnd' :: s -> RunWorld -> b -> MEnv' (s, RunWorld, b)
outputEnd' = \s run b -> do
    gamedata <- resourceGameData 
    let scene = runScene run
        proj2D = sceneProj2D scene
        proj3D = sceneProj3D scene
        modv3D = (cameraViewMat4 $ runCamera run) `mappend` 
                 (cameraModelMat4 $ runCamera run)
    io $ do
        outputSoundEnd' gamedata proj2D proj3D modv3D s run b
        outputScreenEnd' gamedata proj2D proj3D modv3D s run b

    return (s, run, b)


--------------------------------------------------------------------------------
--  

outputLevelMode :: RunWorld -> MEnv' RunWorld
outputLevelMode = \run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "iterationLevelMode"
      outputSoundLevelMode gamedata run
      outputScreenLevelMode gamedata run

    return run


outputPuzzleMode :: RunWorld -> MEnv' RunWorld
outputPuzzleMode = \run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "iterationPuzzleMode"
      outputSoundPuzzleMode gamedata run
      outputScreenPuzzleMode gamedata run

    return run


outputMemoryMode :: RunWorld -> MEnv' RunWorld
outputMemoryMode = \run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "iterationMemoryMode"
      outputSoundMemoryMode gamedata run
      outputScreenMemoryMode gamedata run
  
    return run


outputForeign :: RunWorld -> MEnv' RunWorld
outputForeign = \run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "iterationForeign"
      outputSoundForeign gamedata run
      outputScreenForeign gamedata run

    return run


outputKonami :: RunWorld -> MEnv' RunWorld
outputKonami = \run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "iterationKonami"
      outputSoundKonami gamedata run
      outputScreenKonami gamedata run

    return $ runTweakClear run



--------------------------------------------------------------------------------
--  AtFace


outputAtFace' :: AtFaceState -> RunWorld -> b -> MEnv' (AtFaceState, RunWorld, b)
outputAtFace' = \s run b -> do
    gamedata <- resourceGameData 
    let scene = runScene run
        proj2D = sceneProj2D scene
        proj3D = sceneProj3D scene
        modv3D = (cameraViewMat4 $ runCamera run) `mappend` 
                 (cameraModelMat4 $ runCamera run)
    
    io $ do
        outputSoundAtFace' gamedata proj2D proj3D modv3D s run b 
        outputScreenAtFace' gamedata proj2D proj3D modv3D s run b

    return (s, run, b)


outputAtFace''' :: AtFaceState -> RunWorld -> b -> MEnv' (AtFaceState, RunWorld, b)
outputAtFace''' = \s run b -> do
    gamedata <- resourceGameData 
    let scene = runScene run
        proj2D = sceneProj2D scene
        proj3D = sceneProj3D scene
        modv3D = (cameraViewMat4 $ runCamera run) `mappend` 
                 (cameraModelMat4 $ runCamera run)

    io $ do
        outputSoundAtFace''' gamedata proj2D proj3D modv3D s run b
        outputScreenAtFace''' gamedata proj2D proj3D modv3D s run b
    
    return (s, run, b)


outputAToB :: s -> RunWorld -> b -> MEnv' (s, RunWorld, b)
outputAToB = \s run b -> do
    gamedata <- resourceGameData 
    let scene = runScene run
        proj2D = sceneProj2D scene
        proj3D = sceneProj3D scene
        modv3D = (cameraViewMat4 $ runCamera run) `mappend` 
                 (cameraModelMat4 $ runCamera run)

    io $ do
        outputSoundAToB gamedata proj2D proj3D modv3D s run b
        outputScreenAToB gamedata proj2D proj3D modv3D s run b

    return (s, run, b)




