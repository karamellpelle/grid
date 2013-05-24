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
module Game.Grid.Do
  (
    pathEatWait,
    pathEatContinue,

  ) where

import MyPrelude
import Game
import Game.Grid

import OpenGL
import OpenGL.Helpers


--------------------------------------------------------------------------------
--  onPathNode

-- | eat and wait for defined Turn
pathEatWait :: Path -> IO Path
pathEatWait path = do
    --printPath path
    case pathTurnState path of
        -- continue in direction defined by turn
        (t:ts)    -> do
            path' <- pathEatTurn path t
            return $ path' { pathTurnState = ts }

        -- no defined turn, eat and wait
        []        -> do
            path' <- pathEatTurn path straightTurn
            return $ path' { pathWaiting = True }

    where 
      -- tmp!
      printPath path = do
          let size = pathArraySize path
              begin = pathArrayBegin path
              end = pathArrayEnd path

              out = begin + (end + (size - begin)) `mod` size
              a0 = begin
              a1 = min size out
              b0 = 0
              b1 = (max size out) `mod` size
              pathBegin = pathArrayBegin path
              pathEnd = pathArrayEnd path


          bufVBO <- getBufferSize gl_ARRAY_BUFFER $ pathoutputGLVBO 
                                                  $ pathPathOutput path
          putStrLn $ "pathEatWait"
          putStrLn $ "VBO size:       " ++ show bufVBO ++ " (/ 128 == " 
                                        ++ show (div bufVBO 128) ++ ")" 
          putStrLn $ "pathArraySize:  " ++ show size
          putStrLn $ "pathArrayBegin: " ++ show pathBegin
          putStrLn $ "pathArrayEnd:   " ++ show pathEnd
          putStrLn $ "a0:             " ++ show a0
          putStrLn $ "a1:             " ++ show a1
          putStrLn $ "b0:             " ++ show b0
          putStrLn $ "b1:             " ++ show b1
          putStrLn ""

      getBufferSize tgt buf = do
          glBindBuffer gl_ARRAY_BUFFER buf
          alloca $ \ptr -> do
              glGetBufferParameteriv tgt gl_BUFFER_SIZE ptr
              peek ptr



-- | eat and continue straight ahead if no defined Turn
pathEatContinue :: Path -> IO Path
pathEatContinue path = do
    case pathTurnState path of
        (t:ts)  -> pathEatTurn path t >>= \path -> 
                   return path { pathTurnState = ts }

        []      -> do
            let dx = pathTurnStateX path
                dy = pathTurnStateY path
                mpath   | abs dx < abs dy = mpathY
                        | abs dy < abs dx = mpathX
                        | otherwise       = pathEatStraight path
                mpathX  | dx <= (-sens0)  = pathEat path leftTurn
                        | (sens0) <= dx   = pathEat path rightTurn
                        | otherwise       = pathEatStraight path
                mpathY  | dy <= (-sens0)  = pathEat path upTurn
                        | (sens0) <= dy   = pathEat path downTurn
                        | otherwise       = pathEatStraight path
            mpath 


    where
      pathEatStraight path = 
          pathEatTurn path straightTurn

      pathEat path turn =
          pathEatTurn (path { pathTurnStateHandled = True }) turn 

      sens0 = valueGridControlContinueSens0
      sens1 = valueGridControlContinueSens1





