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
module File.GLFW
  (
    fileStaticData,
    fileDynamicData,
    fileUser,
    fileTmp,

  ) where


import MyPrelude
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import System.Directory

-- from cabal
import Paths_grid


--------------------------------------------------------------------------------
--  TODO: use getBinDir, getDataDir, getLibDir, getLibexecDir getSysconfDir in Paths_grid
--  TODO: use getXdgDirectory?

-- | full path to read-only application data
fileStaticData :: FilePath -> IO FilePath
fileStaticData path = 
    --fmap ("data_fancy/" ++) $ getDataFileName path
#ifdef GRID_STYLE_FANCY
    return $ "data_fancy/" ++ path
#endif
#ifdef GRID_STYLE_PLAIN
    return $ "data_plain/" ++ path
#endif


-- | full path to read-write application data
fileDynamicData :: FilePath -> IO FilePath
fileDynamicData path =
    fmap (++ ("/dynamic/" ++ path)) $ getAppUserDataDirectory "grid" -- FIXME: grid name as cabal


-- | full path to user file directory
fileUser :: FilePath -> IO FilePath
fileUser path = 
    fmap (++ ("/user/" ++ path)) $ getAppUserDataDirectory "grid" -- FIXME: grid name as cabal

-- | full path to tmp file directory
fileTmp :: FilePath -> IO FilePath
fileTmp path = 
    fmap (++ ("/tmp/" ++ path)) $ getAppUserDataDirectory "grid" -- FIXME: grid name as cabal
    --fmap (++ ("/" ++ path)) getTemporaryDirectory 

--TODO:createDirectoryIfMissing True path


