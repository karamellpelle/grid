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
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module File.Binary
  (

    module File.Binary.Reader,
    module File.Binary.Writer,

    module Text.Parsec,
    module Data.Word,
    
  ) where


import MyPrelude
import Text.Parsec
import Data.Word

import File.Binary.Reader
import File.Binary.Writer


--------------------------------------------------------------------------------
--  
-- todo:  * use 'binary' (and 'binary-strict'?) to write own parser library,
--          working similary as 'parsec'. this should fix 'fixme' below.
--        * implement functions for zlib, in order to work with directories
--
-- fixme: * platform endiannes (word8sToWord32, word32ToWord8s)
--        * remove line, columns, and instead talk about offset. then write the 
--          errormessages only with offset.
--        * remove additinal messages to 'fail "my message"'
--        * currently, failure messages are also wrong :(





{-

--------------------------------------------------------------------------------
--  debug

myRead :: Reader a -> [Word8] -> Either ParseError a
myRead ra ws =
    parse ra "myRead" (wrapByteString $ BS.pack ws)


myWrite :: Writer -> [Word8]
myWrite w = 
    BS.unpack $ BS.concat $ BSLazy.toChunks $ runPut w

testReadWrite :: Reader a -> (a -> Writer) -> a -> Either ParseError a
testReadWrite ra f a =
    myRead ra $ myWrite $ f a


minus1 :: [Word8]
minus1 = 
    [0x01, 0, 0, 0x80]

plus0 :: [Word8]
plus0 = [0, 0, 0, 0]


plus1 :: [Word8]
plus1 = [1, 0, 0, 0]

-}
