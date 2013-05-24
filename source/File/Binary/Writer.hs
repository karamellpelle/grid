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
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module File.Binary.Writer
  (
    Writer,

    writeBinary,
    writeBinary',

    wWord8,
    wWord8s,
    wNUL,
    wBool,
    wCString,
    wInt32,
    wUInt32,
    
    --wAlign,
    wCStringAlign, -- tmp!

    word32AsWord8s,


  ) where


import MyPrelude
import Text.Parsec
import System.IO
import Data.Binary.Put

-- Put only works with lazy bytestrings
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS

import Data.Char
import Data.Word
import Data.Bits

import Control.Exception as C

-- this is tmp!
#define PLATFORM_LE

{-
import GHC.Prim
import GHC.Exts
import GHC.Types
import GHC.Int
-}


--------------------------------------------------------------------------------
--  Writer


type Writer =
    Put



--------------------------------------------------------------------------------
--  write

writeBinary :: Writer -> FilePath -> IO (Maybe String)
writeBinary w path =
    C.catch (writeBinary' w path >> return Nothing) $ \e -> 
        return $ Just $ show (e :: IOException)



-- | assuming success, else error
writeBinary':: Writer -> FilePath -> IO ()
writeBinary' w path = do
    C.bracket (openBinaryFile path WriteMode) hClose $ \h -> do
        BS.hPut h $ runPut w




--------------------------------------------------------------------------------
--  

wWord8 :: Word8 -> Writer
wWord8 = 
    putWord8 


wWord8s :: [Word8] -> Writer
wWord8s =
    putLazyByteString . BS.pack -- putLazyByteString?


wNUL :: Writer
wNUL = 
    wWord8 0x00


wBool :: Bool -> Writer
wBool value = 
    case value of
        False   -> wWord8 0x01
        True    -> wWord8 0x02


-- | writing a C-string
wCString :: String -> Writer
wCString str = do
    wWord8s $ map (fI . ord) str
    wNUL

-- fixme: wAlign!
-- tmp solution: assuming n-alignment, padding with zeros
wCStringAlign :: UInt -> String -> Writer
wCStringAlign n str =
    case length' str + 1 of
        size -> do
            wCString str 
            wWord8s (replicate (fI $ (n - size `mod` n) `mod` n) 0x00)


-- | writing a value [-2^31, 2^31)
wInt32 :: Int -> Writer
wInt32 n =
    case word32AsWord8s (convert n) of
        (w0, w1, w2, w3)  -> wWord8s [w0, w1, w2, w3]

    where
{-
      convert (Int# n#) = 
          Word32# (unsafeCoerce# n#)
-}
      convert n = 
          if signum n == (-1) then complement (fI $ abs n) + 1 
                              else (fI $ abs n)


-- | writing a value [0, 2^32). 
wUInt32 :: UInt -> Writer
wUInt32 n = 
    case word32AsWord8s (fI n) of
       (w0, w1, w2, w3) -> wWord8s [w0, w1, w2, w3]


--------------------------------------------------------------------------------
-- 


-- | platform endian -> little endian 
word32AsWord8s :: Word32 -> (Word8, Word8, Word8, Word8)
word32AsWord8s w32 =
#ifdef PLATFORM_LE
    (fI $ 0x000000ff .&. (shiftR w32 0),
     fI $ 0x000000ff .&. (shiftR w32 8),
     fI $ 0x000000ff .&. (shiftR w32 16),
     fI $ 0x000000ff .&. (shiftR w32 24))
#else
     (fI $ 0x000000ff .&. (shiftR w32 24),
      fI $ 0x000000ff .&. (shiftR w32 16),
      fI $ 0x000000ff .&. (shiftR w32 8),
      fI $ 0x000000ff .&. (shiftR w32 0))
#endif 


