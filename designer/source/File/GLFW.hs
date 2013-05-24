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


-- | full path to read-only application data
fileStaticData :: FilePath -> IO FilePath
fileStaticData path = 
    return $ "../data/" ++ path


-- | full path to read-write application data
fileDynamicData :: FilePath -> IO FilePath
fileDynamicData path =
    return $ "../data/dyn/" ++ path

-- | full path to user file directory
fileUser :: FilePath -> IO FilePath
fileUser path = 
    return $ "../data/user" ++ path


-- | full path to tmp file directory
fileTmp :: FilePath -> IO FilePath
fileTmp path = 
    return $  "../data/tmp" ++ path




