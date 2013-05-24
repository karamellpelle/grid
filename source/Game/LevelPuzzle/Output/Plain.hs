module Game.LevelPuzzle.Output.Plain
  (
    outputBeginPlay,
    outputPlay,
    outputPlay',
    outputFailure,
    outputFailure',
    outputComplete,
    outputComplete',
    outputSpecialComplete,
    outputSpecialComplete',

  ) where


import MyPrelude
import Game

import Game.Grid
import Game.Grid.Output
import Game.LevelPuzzle
import Game.LevelPuzzle.Iteration.State
import Game.LevelPuzzle.Output.Plain.Sound
import Game.LevelPuzzle.Output.Plain.Screen
import Game.LevelPuzzle.Output.Plain.State
import Game.Run.RunWorld 

import Linear



--------------------------------------------------------------------------------
--  outputBeginPlay

outputBeginPlay :: LevelPuzzleWorld -> RunWorld -> MEnv' (LevelPuzzleWorld, RunWorld)
outputBeginPlay = \lvl run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "LevelPuzzle.iterationBeginPlay"
      outputSoundBeginPlay gamedata lvl run
      outputScreenBeginPlay gamedata lvl run
    
    return (lvl, run)



--------------------------------------------------------------------------------
--  outputPlay

outputPlay :: LevelPuzzleWorld -> RunWorld -> MEnv' (LevelPuzzleWorld, RunWorld)
outputPlay = \lvl run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "LevelPuzzle.iterationPlay"
      outputSoundPlay gamedata lvl run
      outputScreenPlay gamedata lvl run

      return (lvl, run)


outputPlay' :: s -> LevelPuzzleWorld -> RunWorld -> 
               MEnv' (s, LevelPuzzleWorld, RunWorld) 
outputPlay' = \s lvl run -> do
    gamedata <- resourceGameData
    let Shape wth hth = sceneShape $ runScene run
        proj = mat4Perspective valuePerspectiveFOVY
                               (wth / hth)
                               valuePerspectiveNear
                               valuePerspectiveFar
        modv = mat4SceneCamera $ levelpuzzleGrid lvl
        normal = modv
    io $ do
      outputSoundPlay' gamedata proj modv normal s lvl run
      outputScreenPlay' gamedata proj modv normal s lvl run

      return (s, lvl, run)


--------------------------------------------------------------------------------
--  outputFailure

outputFailure :: LevelPuzzleWorld -> RunWorld -> 
                 MEnv' (LevelPuzzleWorld, RunWorld)
outputFailure = \lvl run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "LevelPuzzle.iterationFailure"
      outputSoundFailure gamedata lvl run
      outputScreenFailure gamedata lvl run

      return (lvl, run)


outputFailure' :: FailureS -> LevelPuzzleWorld -> RunWorld -> 
                  MEnv' (FailureS, LevelPuzzleWorld, RunWorld)
outputFailure' = \s lvl run -> do
    gamedata <- resourceGameData
    let Shape wth hth = sceneShape $ runScene run
        proj = mat4Perspective valuePerspectiveFOVY
                               (wth / hth)
                               valuePerspectiveNear
                               valuePerspectiveFar
        modv = mat4SceneCamera $ levelpuzzleGrid lvl
        normal = modv
    io $ do
      outputSoundFailure' gamedata proj modv normal s lvl run
      outputScreenFailure' gamedata proj modv normal s lvl run

      return (s, lvl, run)


--------------------------------------------------------------------------------
--  outputComplete


outputComplete :: LevelPuzzleWorld -> RunWorld -> 
                  MEnv' (LevelPuzzleWorld, RunWorld)
outputComplete = \lvl run -> do
    gamedata <- resourceGameData
    io $ do
      (lvl', run') <- stateSetComplete lvl run

      putStrLn "LevelPuzzle.iterationComplete"
      outputSoundComplete gamedata lvl' run'
      outputScreenComplete gamedata lvl' run'

      return (lvl', run')


outputComplete' :: s -> LevelPuzzleWorld -> RunWorld -> 
                   MEnv' (s, LevelPuzzleWorld, RunWorld)
outputComplete' = \s lvl run -> do 
    gamedata <- resourceGameData
    let Shape wth hth = sceneShape $ runScene run
        proj = mat4Perspective valuePerspectiveFOVY
                               (wth / hth)
                               valuePerspectiveNear
                               valuePerspectiveFar
        modv = mat4SceneCamera $ levelpuzzleGrid lvl
        normal = modv
    io $ do
      outputSoundComplete' gamedata proj modv normal s lvl run
      outputScreenComplete' gamedata proj modv normal s lvl run
     
      stateStepComplete' s lvl run



--------------------------------------------------------------------------------
--  outputSpecialComplete


outputSpecialComplete :: LevelPuzzleWorld -> RunWorld -> 
                         MEnv' (LevelPuzzleWorld, RunWorld)
outputSpecialComplete = \lvl run -> do
    gamedata <- resourceGameData
    io $ do
      (lvl', run') <- stateSetSpecialComplete lvl run

      putStrLn "LevelPuzzle.iterationSpecialComplete"
      outputSoundSpecialComplete gamedata lvl' run'
      outputScreenSpecialComplete gamedata lvl' run'

      return (lvl', run')


outputSpecialComplete' :: s -> LevelPuzzleWorld -> RunWorld -> 
                          MEnv' (s, LevelPuzzleWorld, RunWorld)
outputSpecialComplete' = \s lvl run -> do 
    gamedata <- resourceGameData
    let Shape wth hth = sceneShape $ runScene run
        proj = mat4Perspective valuePerspectiveFOVY
                               (wth / hth)
                               valuePerspectiveNear
                               valuePerspectiveFar
        modv = mat4SceneCamera $ levelpuzzleGrid lvl
        normal = modv
    io $ do
      outputSoundSpecialComplete' gamedata proj modv normal s lvl run
      outputScreenSpecialComplete' gamedata proj modv normal s lvl run

      stateStepSpecialComplete' s lvl run


