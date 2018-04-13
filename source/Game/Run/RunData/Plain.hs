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
module Game.Run.RunData.Plain
  (
    RunData (..),
    
    loadRunData,
    unloadRunData,

    module Game.Run.RunData.Plain.ShadeScene,
    module Game.Run.RunData.Plain.ShadeSceneBegin,
    module Game.Run.RunData.Plain.ShadeScenePause,
    module Game.Run.RunData.Plain.ShadeSceneKonami,
    module Game.Run.RunData.Plain.ShadeCube,
    module Game.Run.RunData.Plain.ShadeCorners,
    module Game.Run.RunData.Plain.ShadeScreenshot,
    module Game.Run.RunData.Plain.SoundRun,
    module Game.Run.RunData.Plain.SoundScene,

  ) where

import MyPrelude
import File

import OpenGL
import OpenGL.Helpers

import Game.Run.RunData.Plain.ShadeScene
import Game.Run.RunData.Plain.ShadeSceneBegin
import Game.Run.RunData.Plain.ShadeScenePause
import Game.Run.RunData.Plain.ShadeSceneKonami
import Game.Run.RunData.Plain.ShadeCube
import Game.Run.RunData.Plain.ShadeCorners
import Game.Run.RunData.Plain.ShadeScreenshot
import Game.Run.RunData.Plain.SoundRun
import Game.Run.RunData.Plain.SoundScene



data RunData =
    RunData
    {
        rundataShadeScene :: !ShadeScene,
        rundataShadeSceneBegin :: !ShadeSceneBegin,
        rundataShadeScenePause :: !ShadeScenePause,
        rundataShadeSceneKonami :: !ShadeSceneKonami,
        rundataShadeCube :: !ShadeCube,
        rundataShadeCorners :: !ShadeCorners,
        rundataShadeScreenshot :: !ShadeScreenshot,
        rundataSoundRun :: !SoundRun,
        rundataSoundScene :: !SoundScene
    }



loadRunData :: IO RunData
loadRunData = do
    shScene <- loadShadeScene
    shSceneBegin <- loadShadeSceneBegin
    shScenePause <- loadShadeScenePause
    shSceneKonami <- loadShadeSceneKonami
    shCube <- loadShadeCube
    shCorners <- loadShadeCorners
    shScreenshot <- loadShadeScreenshot
    sndRun <- loadSoundRun
    sndScene <- loadSoundScene

    return $  RunData
              {
                  rundataShadeScene = shScene,
                  rundataShadeSceneBegin = shSceneBegin,
                  rundataShadeScenePause = shScenePause,
                  rundataShadeSceneKonami = shSceneKonami,
                  rundataShadeCube = shCube,
                  rundataShadeCorners = shCorners,
                  rundataShadeScreenshot = shScreenshot,
                  rundataSoundRun = sndRun,
                  rundataSoundScene = sndScene
              }

unloadRunData :: RunData -> IO ()
unloadRunData rundata = do
    unloadSoundScene $ rundataSoundScene rundata
    unloadSoundRun $ rundataSoundRun rundata
    unloadShadeScreenshot $ rundataShadeScreenshot rundata
    unloadShadeCorners $ rundataShadeCorners rundata
    unloadShadeCube $ rundataShadeCube rundata
    unloadShadeSceneKonami $ rundataShadeSceneKonami rundata
    unloadShadeScenePause $ rundataShadeScenePause rundata
    unloadShadeSceneBegin $ rundataShadeSceneBegin rundata
    unloadShadeScene $ rundataShadeScene rundata


