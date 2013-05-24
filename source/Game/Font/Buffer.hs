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
module Game.Font.Buffer
  (
    writeDefaultPosLen,
    writeDefaultStencilCoord2D,
    writeDefaultStencilCoord3D,
    
    writePosLen,
    writePosStencilCoord2D,
    writePosStencilCoord3D,
    writeStencilCoord2D,
    writeStencilCoord3D,

  ) where


import MyPrelude
import File
import Data.Char
import Game.Font.FontData

import OpenGL
import OpenGL.Helpers


--------------------------------------------------------------------------------
--  buffers for Default


writeDefaultPosLen :: Ptr a -> UInt -> IO ()
writeDefaultPosLen ptr len =
    helper ptr 0 len
    where
      helper ptr x 0 =
          return ()
      helper ptr x len = do
          let x' = x + 1 :: GLushort
          pokeByteOff ptr 0   x
          pokeByteOff ptr 2   (1 :: GLushort)
          pokeByteOff ptr 4   x
          pokeByteOff ptr 6   (0 :: GLushort)
          pokeByteOff ptr 8   x'
          pokeByteOff ptr 10  (1 :: GLushort)
          pokeByteOff ptr 12  x'
          pokeByteOff ptr 14  (0 :: GLushort)

          helper (plusPtr ptr 16) x' (len - 1)


writeDefaultStencilCoord2D :: FontData -> Ptr a -> String -> IO UInt
writeDefaultStencilCoord2D fd ptr str = 
    helper ptr str 0
    where
      helper ptr [] len =
          return len
      helper ptr (c:cs) len = do
          let ix = fI (ord c) - fontdataOffset fd
              (ixY, ixX) = ix `divMod` fontdataCharsX fd
              ixX' = fI ixX
              ixY' = fI ixY
              s0 = fontdataCharX fd * ixX' + fontdataCharPadX fd
              s1 = fontdataCharX fd * (ixX' + 1) - fontdataCharPadX fd
              t0 = fontdataCharY fd * ixY' + fontdataCharPadY fd
              t1 = fontdataCharY fd * (ixY' + 1) - fontdataCharPadY fd
          pokeByteOff ptr 0   s0
          pokeByteOff ptr 2   t1

          pokeByteOff ptr 4   s0
          pokeByteOff ptr 6   t0

          pokeByteOff ptr 8   s1
          pokeByteOff ptr 10  t1

          pokeByteOff ptr 12  s1
          pokeByteOff ptr 14  t0

          helper (plusPtr ptr 16) cs (len + 1)


      

writeDefaultStencilCoord3D :: FontData -> Ptr a -> String -> IO UInt
writeDefaultStencilCoord3D fd ptr str = 
    helper ptr str 0
    where
      helper ptr [] len =
          return len
      helper ptr (c:cs) len = do
          let ix = fI (ord c) - fontdataOffset fd
              (ixY, ixX) = ix `divMod` fontdataCharsX fd
              ixX' = fI ixX
              ixY' = fI ixY
              s0 = fontdataCharX fd * ixX' + fontdataCharPadX fd
              s1 = fontdataCharX fd * (ixX' + 1) - fontdataCharPadX fd
              t0 = fontdataCharY fd * ixY' + fontdataCharPadY fd
              t1 = fontdataCharY fd * (ixY' + 1) - fontdataCharPadY fd
          pokeByteOff ptr 0   s0
          pokeByteOff ptr 2   t0

          pokeByteOff ptr 4   s0
          pokeByteOff ptr 6   t1

          pokeByteOff ptr 8   s1
          pokeByteOff ptr 10  t0

          pokeByteOff ptr 12  s1
          pokeByteOff ptr 14  t1

          helper (plusPtr ptr 16) cs (len + 1)



--------------------------------------------------------------------------------
--  buffers for FontObject


writePosLen :: Ptr a -> UInt -> IO ()
writePosLen ptr len =
    helper ptr 0 len
    where
      helper ptr x 0 =
          return ()
      helper ptr x len = do
          let x' = x + 1 :: GLushort
          pokeByteOff ptr 0   x
          pokeByteOff ptr 2   (1 :: GLushort)
          pokeByteOff ptr 8   x
          pokeByteOff ptr 10  (0 :: GLushort)
          pokeByteOff ptr 16  x'
          pokeByteOff ptr 18  (1 :: GLushort)
          pokeByteOff ptr 24  x'
          pokeByteOff ptr 26  (0 :: GLushort)

          helper (plusPtr ptr 32) x' (len - 1)



writePosStencilCoord2D :: FontData -> Ptr a -> String -> IO UInt
writePosStencilCoord2D fd ptr str = 
    helper ptr str 0 0
    where
      helper ptr [] x len =
          return len
      helper ptr (c:cs) x len = do
          let ix = fI (ord c) - fontdataOffset fd
              (ixY, ixX) = ix `divMod` fontdataCharsX fd
              ixX' = fI ixX
              ixY' = fI ixY
              s0 = fontdataCharX fd * ixX' + fontdataCharPadX fd
              s1 = fontdataCharX fd * (ixX' + 1) - fontdataCharPadX fd
              t0 = fontdataCharY fd * ixY' + fontdataCharPadY fd
              t1 = fontdataCharY fd * (ixY' + 1) - fontdataCharPadY fd
              x' = x + 1 :: GLushort
          pokeByteOff ptr 0   x
          pokeByteOff ptr 2   (1 :: GLushort)
          pokeByteOff ptr 4   s0
          pokeByteOff ptr 6   t1

          pokeByteOff ptr 8   x
          pokeByteOff ptr 10  (0 :: GLushort)
          pokeByteOff ptr 12  s0
          pokeByteOff ptr 14  t0

          pokeByteOff ptr 16  x'
          pokeByteOff ptr 18  (1 :: GLushort)
          pokeByteOff ptr 20  s1
          pokeByteOff ptr 22  t1

          pokeByteOff ptr 24  x'
          pokeByteOff ptr 26  (0 :: GLushort)
          pokeByteOff ptr 28  s1
          pokeByteOff ptr 30  t0

          helper (plusPtr ptr 32) cs x' (len + 1)

          
writePosStencilCoord3D :: FontData -> Ptr a -> String -> IO UInt
writePosStencilCoord3D fd ptr str = 
    helper ptr str 0 0
    where
      helper ptr [] x len =
          return len
      helper ptr (c:cs) x len = do
          let ix = fI (ord c) - fontdataOffset fd
              (ixY, ixX) = ix `divMod` fontdataCharsX fd
              ixX' = fI ixX
              ixY' = fI ixY
              s0 = fontdataCharX fd * ixX' + fontdataCharPadX fd
              s1 = fontdataCharX fd * (ixX' + 1) - fontdataCharPadX fd
              t0 = fontdataCharY fd * ixY' + fontdataCharPadY fd
              t1 = fontdataCharY fd * (ixY' + 1) - fontdataCharPadY fd
              x' = x + 1 :: GLushort
          pokeByteOff ptr 0   x
          pokeByteOff ptr 2   (1 :: GLushort)
          pokeByteOff ptr 4   s0
          pokeByteOff ptr 6   t0

          pokeByteOff ptr 8   x
          pokeByteOff ptr 10  (0 :: GLushort)
          pokeByteOff ptr 12  s0
          pokeByteOff ptr 14  t1

          pokeByteOff ptr 16  x'
          pokeByteOff ptr 18  (1 :: GLushort)
          pokeByteOff ptr 20  s1
          pokeByteOff ptr 22  t0

          pokeByteOff ptr 24  x'
          pokeByteOff ptr 26  (0 :: GLushort)
          pokeByteOff ptr 28  s1
          pokeByteOff ptr 30  t1

          helper (plusPtr ptr 32) cs x' (len + 1)


writeStencilCoord2D :: FontData -> Ptr a -> String -> IO UInt
writeStencilCoord2D fd ptr str = 
    helper ptr str 0
    where
      helper ptr [] len =
          return len
      helper ptr (c:cs) len = do
          let ix = fI (ord c) - fontdataOffset fd
              (ixY, ixX) = ix `divMod` fontdataCharsX fd
              ixX' = fI ixX
              ixY' = fI ixY
              s0 = fontdataCharX fd * ixX' + fontdataCharPadX fd
              s1 = fontdataCharX fd * (ixX' + 1) - fontdataCharPadX fd
              t0 = fontdataCharY fd * ixY' + fontdataCharPadY fd
              t1 = fontdataCharY fd * (ixY' + 1) - fontdataCharPadY fd
          pokeByteOff ptr 4   s0
          pokeByteOff ptr 6   t1

          pokeByteOff ptr 12  s0
          pokeByteOff ptr 14  t0

          pokeByteOff ptr 20  s1
          pokeByteOff ptr 22  t1

          pokeByteOff ptr 28  s1
          pokeByteOff ptr 30  t0

          helper (plusPtr ptr 32) cs (len + 1)


      

writeStencilCoord3D :: FontData -> Ptr a -> String -> IO UInt
writeStencilCoord3D fd ptr str = 
    helper ptr str 0
    where
      helper ptr [] len =
          return len
      helper ptr (c:cs) len = do
          let ix = fI (ord c) - fontdataOffset fd
              (ixY, ixX) = ix `divMod` fontdataCharsX fd
              ixX' = fI ixX
              ixY' = fI ixY
              s0 = fontdataCharX fd * ixX' + fontdataCharPadX fd
              s1 = fontdataCharX fd * (ixX' + 1) - fontdataCharPadX fd
              t0 = fontdataCharY fd * ixY' + fontdataCharPadY fd
              t1 = fontdataCharY fd * (ixY' + 1) - fontdataCharPadY fd
          pokeByteOff ptr 4   s0
          pokeByteOff ptr 6   t0

          pokeByteOff ptr 12  s0
          pokeByteOff ptr 14  t1

          pokeByteOff ptr 20  s1
          pokeByteOff ptr 22  t0

          pokeByteOff ptr 28  s1
          pokeByteOff ptr 30  t1

          helper (plusPtr ptr 32) cs (len + 1)


