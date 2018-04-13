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
module Game.Run.RunData.Fancy
  (
    RunData (..),
    
    loadRunData,
    unloadRunData,

    module Game.Run.RunData.Fancy.ShadeScene,
    module Game.Run.RunData.Fancy.ShadeSceneBegin,
    module Game.Run.RunData.Fancy.ShadeScenePause,
    module Game.Run.RunData.Fancy.ShadeSceneKonami,
    module Game.Run.RunData.Fancy.ShadeCube,
    module Game.Run.RunData.Fancy.ShadeScreenshot,
    module Game.Run.RunData.Fancy.SoundRun,
    module Game.Run.RunData.Fancy.SoundScene,
    module Game.Run.RunData.Fancy.SceneData,
    module Game.Run.RunData.Fancy.CornerData,

  ) where

import MyPrelude
import File

import OpenGL
import OpenGL.Helpers

import Game.Run.RunData.Fancy.ShadeScene
import Game.Run.RunData.Fancy.ShadeSceneBegin
import Game.Run.RunData.Fancy.ShadeScenePause
import Game.Run.RunData.Fancy.ShadeSceneKonami
import Game.Run.RunData.Fancy.ShadeCube
import Game.Run.RunData.Fancy.ShadeScreenshot
import Game.Run.RunData.Fancy.SoundRun
import Game.Run.RunData.Fancy.SoundScene
import Game.Run.RunData.Fancy.SceneData
import Game.Run.RunData.Fancy.CornerData



data RunData =
    RunData
    {
        rundataShadeScene :: !ShadeScene,
        rundataShadeSceneBegin :: !ShadeSceneBegin,
        rundataShadeScenePause :: !ShadeScenePause,
        rundataShadeSceneKonami :: !ShadeSceneKonami,
        rundataShadeCube :: !ShadeCube,
        rundataShadeScreenshot :: !ShadeScreenshot,
        rundataSoundRun :: !SoundRun,
        rundataSoundScene :: !SoundScene,
        rundataSceneData :: !SceneData,
        rundataCornerData :: !CornerData
    }



loadRunData :: IO RunData
loadRunData = do
    shScene <- loadShadeScene
    shSceneBegin <- loadShadeSceneBegin
    shScenePause <- loadShadeScenePause
    shSceneKonami <- loadShadeSceneKonami
    shCube <- loadShadeCube
    shScreenshot <- loadShadeScreenshot
    sndRun <- loadSoundRun
    sndScene <- loadSoundScene
    scenedata <- loadSceneData
    cornerdata <- loadCornerData

    return $  RunData
              {
                  rundataShadeScene = shScene,
                  rundataShadeSceneBegin = shSceneBegin,
                  rundataShadeScenePause = shScenePause,
                  rundataShadeSceneKonami = shSceneKonami,
                  rundataShadeCube = shCube,
                  rundataShadeScreenshot = shScreenshot,
                  rundataSoundRun = sndRun,
                  rundataSoundScene = sndScene,
                  rundataSceneData = scenedata,
                  rundataCornerData = cornerdata
              }

unloadRunData :: RunData -> IO ()
unloadRunData rundata = do
    unloadCornerData $ rundataCornerData rundata
    unloadSceneData $ rundataSceneData rundata
    unloadSoundScene $ rundataSoundScene rundata
    unloadSoundRun $ rundataSoundRun rundata
    unloadShadeScreenshot $ rundataShadeScreenshot rundata
    unloadShadeCube $ rundataShadeCube rundata
    unloadShadeSceneKonami $ rundataShadeSceneKonami rundata
    unloadShadeScenePause $ rundataShadeScenePause rundata
    unloadShadeSceneBegin $ rundataShadeSceneBegin rundata
    unloadShadeScene $ rundataShadeScene rundata


