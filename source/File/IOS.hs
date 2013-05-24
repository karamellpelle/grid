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
module File.IOS
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


-- | fixme: do not use fixed arrays, instead peek foreign values
--          as in MEnv.Players.IOS !
valueMaxFilePathLength :: UInt
valueMaxFilePathLength = 
    255

foreign import ccall unsafe "ios_fileStaticData" ios_fileStaticData
    :: CString -> CString -> CUInt -> IO CUInt

-- | full path to read-only application data
fileStaticData :: FilePath -> IO FilePath
fileStaticData path = 
    withCString path $ \ptrNameExt -> 
      allocaArray0 (fI valueMaxFilePathLength) $ \ptrDst -> do
        ios_fileStaticData ptrNameExt ptrDst 
                               (fI $ valueMaxFilePathLength + 1) >>= \value -> 
              case value of 
                  0   -> error $ "fileStaticData: no file '" ++ path ++ "'"
                  _   -> peekCString ptrDst




foreign import ccall unsafe "ios_fileDynamicData" ios_fileDynamicData
    :: CString -> CString -> CUInt -> IO CUInt

-- | full path to read-write application data
fileDynamicData :: FilePath -> IO FilePath
fileDynamicData path =
    withCString path $ \ptrNameExt -> 
      allocaArray0 (fI valueMaxFilePathLength) $ \ptrDst -> do
        ios_fileDynamicData ptrNameExt ptrDst 
                                (fI $ valueMaxFilePathLength + 1) >>= \value -> 
            case value of
                0   -> error $ "fileDynamicData: no file '" ++ path ++ "'"
                _   -> peekCString ptrDst



foreign import ccall unsafe "ios_fileUser" ios_fileUser
    :: CString -> CString -> CUInt -> IO CUInt

-- | full path to user file directory
fileUser :: FilePath -> IO FilePath
fileUser path = 
    withCString path $ \ptrNameExt -> 
      allocaArray0 (fI valueMaxFilePathLength) $ \ptrDst -> do
        ios_fileUser ptrNameExt ptrDst 
                         (fI valueMaxFilePathLength) >>= \value -> 
            case value of
                0   -> error $ "fileUser: no file '" ++ path ++ "'"
                _   -> peekCString ptrDst




foreign import ccall unsafe "ios_fileTmp" ios_fileTmp
    :: CString -> CString -> CUInt -> IO CUInt

-- | full path to tmp file directory
fileTmp :: FilePath -> IO FilePath
fileTmp path = 
    withCString path $ \ptrNameExt -> 
      allocaArray0 (fI valueMaxFilePathLength) $ \ptrDst -> do
        ios_fileTmp ptrNameExt ptrDst 
                    (fI valueMaxFilePathLength) >>= \value -> case value of
                0   -> error $ "fileTmp: no file '" ++ path ++ "'"
                _   -> peekCString ptrDst





