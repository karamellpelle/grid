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
module Game.Run.RunWorld.Screenshot.Fancy
  (
    Screenshot (..),
    
    makeScreenshot,
    remakeScreenshot,
    destroyScreenshot,

  ) where


import MyPrelude
import Game

import OpenGL
import OpenGL.Helpers


data Screenshot =
    Screenshot
    {
        screenshotTex :: !GLuint,
        screenshotWth :: !UInt,
        screenshotHth :: !UInt
    }


--------------------------------------------------------------------------------
--  

makeScreenshot :: FilePath -> MEnv' Screenshot
makeScreenshot path = io $ do
    tex <- bindNewTex gl_TEXTURE_2D
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_LINEAR --_MIPMAP_LINEAR
    --glGenerateMipmap gl_TEXTURE_2D

    (wth, hth) <- loadTexPreMult gl_TEXTURE_2D gl_RGBA path -- fixme: intfmt

    return Screenshot
           {
              screenshotTex = tex,
              screenshotWth = wth,
              screenshotHth = hth
           }



remakeScreenshot :: FilePath -> Screenshot -> MEnv' Screenshot
remakeScreenshot path sshot = io $ do
    glBindTexture gl_TEXTURE_2D $ screenshotTex sshot
    (wth, hth) <- loadTexPreMult gl_TEXTURE_2D gl_RGBA path -- fixme: intfmt
    --glGenerateMipmap gl_TEXTURE_2D
    return sshot
           {
              screenshotWth = wth,
              screenshotHth = hth
           }


destroyScreenshot :: Screenshot -> MEnv' ()
destroyScreenshot sshot = io $ do
    delTex $ screenshotTex sshot
