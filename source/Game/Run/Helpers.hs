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
module Game.Run.Helpers
  (
    runCamera,
    runClearEvents,
    runModifyGrid,
    runModifyCamera,
    runModifyMessage,
    runSetMessageAtRef,
    runMessagePush,
    runMessagePushClear,
    runMessageClearPush,
    runMessageClear,
    runMessageWait,
    runMessageAbort,
    runMessageIsComplete,

    runCameraCmdsIsComplete,
    runSetCameraCmds,
    runPushCameraCmds,
    runSetTurnView,

    viewFromRunFace,
    viewFromShape, 
    
    faceFromTurn,
    faceFromDir,
    turnFromFace,
    
    commentIterationBegin,
    commentKonami,
    commentLevelPuzzleEscape,
    commentMemoryEscape,
    commentAbout,
    
    module Game.Run.Helpers.Message,

#ifdef GRID_STYLE_FANCY
    module Game.Run.Helpers.Fancy,
#endif
#ifdef GRID_STYLE_PLAIN
    module Game.Run.Helpers.Plain,
#endif

  ) where


import MyPrelude
import System.Random
import Game

import Game.Run.RunWorld
import Game.Run.RunData
import Game.Grid
import Game.Run.Helpers.Message

import OpenGL
import OpenGL.Helpers

#ifdef GRID_STYLE_FANCY
import Game.Run.Helpers.Fancy
#endif
#ifdef GRID_STYLE_PLAIN
import Game.Run.Helpers.Plain
#endif



runClearEvents :: RunWorld -> RunWorld
runClearEvents run =
    run { runEvents = [] }


--------------------------------------------------------------------------------
--  


runCamera :: RunWorld -> Camera
runCamera = 
    gridCamera . runGrid 

runModifyGrid :: RunWorld -> (GridWorld -> GridWorld) -> RunWorld
runModifyGrid run f =
    run { runGrid = f $ runGrid run }

runModifyCamera :: RunWorld -> (Camera -> Camera) -> RunWorld
runModifyCamera run f =
    runModifyGrid run $ \grid -> gridModifyCamera grid f

runModifyMessage :: RunWorld -> (Message -> Message) -> RunWorld
runModifyMessage run f =
    run { runMessage = f $ runMessage run }

runMessageIsComplete :: RunWorld -> Bool
runMessageIsComplete run = 
    let msg = runMessage run
    in  null (messageAS msg) &&
        null (messageASS msg) &&
        null (messageASSS msg)


--------------------------------------------------------------------------------
--  

runCameraCmdsIsComplete :: RunWorld -> Bool
runCameraCmdsIsComplete run = 
    gridCameraCmdsIsComplete $ runGrid run


runSetCameraCmds :: RunWorld -> [CameraCommand] -> RunWorld
runSetCameraCmds run cmds = 
    runModifyGrid run $ \grid -> gridSetCameraCmds grid cmds


runPushCameraCmds :: RunWorld -> [CameraCommand] -> RunWorld
runPushCameraCmds run cmds =
    runModifyGrid run $ \grid -> gridPushCameraCmds grid cmds


runSetTurnView :: RunWorld -> Turn -> View -> RunWorld
runSetTurnView run turn view = 
    runModifyCamera (run { runTurn = turn }) $ \cam -> 
        cameraSetTurnView cam turn view




--------------------------------------------------------------------------------
--  

runSetMessageAtRef :: RunWorld -> Segment -> RunWorld
runSetMessageAtRef run seg = 
    runModifyMessage run $ \msg -> msg { messageRef = seg }


runMessagePush :: RunWorld -> String -> RunWorld
runMessagePush run str = 
    runModifyMessage run $ \msg -> msg { messageASSS = messageASSS msg ++ [str] }


runMessagePushClear :: RunWorld -> String -> RunWorld
runMessagePushClear run str = 
    runMessagePush run $ str ++ replicate 64 '\t'    


runMessageClearPush :: RunWorld -> String -> RunWorld
runMessageClearPush run str = 
    runMessagePush run $ replicate 64 '\t' ++ str


runMessageClear :: RunWorld -> RunWorld
runMessageClear run = 
    runMessagePush run $ replicate 64 '\t'



-- | fixme: fix this function!
runMessageWait :: RunWorld -> Tick -> RunWorld
runMessageWait run ticks =
    runMessagePush run $ replicate wait '\0'
    where
      wait = fI $ div (truncate (ticks * fI valueRunMessageSegsPerTick)) 
                      (valueRunMessageSegsPerWait)


runMessageAbort :: RunWorld -> RunWorld
runMessageAbort run = 
    runModifyMessage run messageClear

    where
      messageClear msg = 
          msg
          {
              messageA = mempty,
              messageAS = mempty,
              messageASS = mempty,
              messageASSS = mempty
          }


--------------------------------------------------------------------------------
--  


viewFromRunFace :: RunWorld -> View
viewFromRunFace = 
    viewFromShape . sceneShape . runScene 


viewFromShape :: Shape -> View
viewFromShape (Shape wth hth) = 
    View 0.0 0.0 $ valueRunCubeRadius * 
                   (1.0 + hth / (wth * (tan $ 0.5 * valuePerspectiveFOVY)))

    

faceFromTurn :: Turn -> Face
faceFromTurn =
    faceFromDir . direction


faceFromDir :: Dir -> Face
faceFromDir dir =
    case dir of
        Dir 1 0 0     -> FaceLevelMode
        Dir (-1) 0 0  -> FacePuzzleMode
        Dir 0 0 1     -> FaceMemoryMode
        Dir 0 0 (-1)  -> FaceForeign
        Dir 0 1 0     -> FaceAbout
        Dir 0 (-1) 0  -> FaceSettings
        dir           -> error $ "dirFace: no such direction!"


turnFromFace :: Face -> Turn
turnFromFace face = 
    case face of
        FaceLevelMode   -> turnFaceLevelMode

        FacePuzzleMode  -> turnFacePuzzleMode  
                                             
        FaceMemoryMode  -> turnFaceMemoryMode 
                                             
        FaceForeign     -> turnFaceForeign   
                                             
        FaceAbout       -> turnFaceAbout    
                                             
        FaceSettings    -> turnFaceSettings




--------------------------------------------------------------------------------
--  Comment (the game wants to talk to the player)

commentIterationBegin :: RunWorld -> MEnv' RunWorld
commentIterationBegin run = do
    return $ runMessagePushClear run "wellcome to the Haskell world"


commentKonami :: RunWorld -> MEnv' RunWorld
commentKonami run = 
    return $ runMessagePushClear run "   Lambda Lover!"


commentLevelPuzzleEscape :: RunWorld -> MEnv' RunWorld
commentLevelPuzzleEscape run = 
    --return $ runMessagePushClear run "Escaped from LevelPuzzle"
    return run


commentMemoryEscape :: RunWorld -> MEnv' RunWorld
commentMemoryEscape run = 
    --return $ runMessagePushClear run "Escaped from PuzzleMode"
    return run


commentAbout :: RunWorld -> MEnv' RunWorld
commentAbout run = 
    return $ runMessagePush run "karamellpelle@hotmail.com"



