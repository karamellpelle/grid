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
module MyPrelude
  (
    UInt,
    length',

    foldl',
    smooth,
    fI,
    rTF,
    tuples2,
    tuples3,
    io,
    keepInside,
    range,
    tau,
    cossin,
    modulo,
    moduloTau,
    moduloNegTauTau,
    succMod,
    predMod,
    invMod,
    (>>>),

    module Data.Monoid,
    module Control.Monad, 

#ifdef DEBUG
    module Debug,
#endif

  ) where

import Data.List
import Data.Word
import Data.Monoid
import Control.Monad
import Control.Monad.Trans

#ifdef DEBUG
import Debug
#endif


--------------------------------------------------------------------------------
--  

-- | using Int's is ridiculous, it is the same as using complex numbers. 
--   since "sets" is a natural logical concept, we have a data structure for
--   that, namely UInt. or should we use Peano numbers? 
--   the natural numbers \mathbf{N} is the structure "sets of finite size"
--   the integer numbers \mathbf{Z} is the structure "steps in direction"
type UInt = 
    Word


--------------------------------------------------------------------------------
--  

length' :: [a] -> UInt
length' = 
    fI . length

smooth :: Float -> Float -> Float -> Float
smooth x x' alpha =
    (1 - alpha) * x + alpha * x'


--------------------------------------------------------------------------------
--  

fI :: (Integral a, Num b) => a -> b
fI =
    fromIntegral

rTF :: (Real a, Fractional b) => a -> b
rTF =
    realToFrac


--------------------------------------------------------------------------------
--  

tuples2 :: [a] -> [b] -> [(a,b)]
tuples2 as bs = do
    a <- as
    b <- bs
    return (a, b)

tuples3 :: [a] -> [b] -> [c] -> [(a, b, c)]
tuples3 as bs cs = do
    a <- as
    b <- bs
    c <- cs
    return (a, b, c)



--------------------------------------------------------------------------------
--  

-- | liftIO 
io :: MonadIO m => IO a -> m a 
io =
    liftIO


--------------------------------------------------------------------------------
--  

-- | put 'x' inside the closed interval [min, max]
keepInside :: Ord a => a -> a -> a -> a
keepInside min max x
    | x <= min  = min
    | max <= x  = max
    | otherwise = x




--------------------------------------------------------------------------------
-- 

-- | range [x, y)
--   fixme/note: Enum throws error if succ maxBound/pred minBound. for
--               example, range 0 256 gives error for Word8!
--range :: (Enum a, Eq a) => a -> a -> [a]
--range x y =
--    if x == y then []
--    else x : range (succ x) y
range :: (Eq a, Num a) => a -> a -> [a]
range b e =
    if b == e then []
              else b : range (b + 1) e

--------------------------------------------------------------------------------
--  

-- | 1.0 is the unit when working in the plane \mathbf{R}^2. spheres are defined by radius. 
--   it is natural to have a symbol for the number "circumference of a unit sphere.
--   (this at least much more natural than the symbol 'pi')
tau :: Floating a => a
tau = 2.0 * pi

--------------------------------------------------------------------------------
--  

-- | (dx, dy) of angle
cossin :: Float -> (Float, Float)
cossin a =
    (cos a, sin a)


--------------------------------------------------------------------------------
--  

-- | put 'p' inside the interval [x, y)
-- Interal, Num, RealFrac
modulo :: Float -> Float -> Float -> Float
modulo x y p =
    let p' = p - x
        d = y - x
    in  p' - d * fromIntegral (floor (p' / d) :: Int)

-- | put 'p' insinde [0, tau)
moduloTau :: Float -> Float
moduloTau p =
    p - 6.2831855 * fromIntegral (floor (p * 0.15915494) :: Int)

-- | put 'p' inside (tau, tau)
moduloNegTauTau :: Float -> Float
moduloNegTauTau p =
    p - 6.2831855 * fromIntegral (truncate (p * 0.15915494) :: Int)



succMod :: UInt -> UInt -> UInt
succMod m n =
    (n + 1) `mod` m

predMod :: UInt -> UInt -> UInt
predMod m n = 
    (n + (m - 1)) `mod` m

invMod :: UInt -> UInt -> UInt 
invMod m n = 
    (m - n) `mod` n


--------------------------------------------------------------------------------
--  

-- | we define combinator for our arrows (Kleisli arrows).
(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>>>) tab tbc =
    \a -> tab a >>= tbc
infixr 1 >>>


--------------------------------------------------------------------------------
--  








