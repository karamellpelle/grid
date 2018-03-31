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
module Game.Iteration
  (
    Iteration,
    IterationStack,
    iteration,
    makeIteration,

    Iteration',
    IterationStack',
    iteration',
    makeIteration',

    Iteration'',
    IterationStack'',
    iteration'',
    makeIteration'',

    defaultIteration,
    iterateABStack,
    nullABStack,
    noOutput,
    noStep,

    modifyBefore,
    modifyAfter,
    localWorldA,
    saveWorldA,
    chooseTopAfter,

  ) where


import MyPrelude
import Game.MEnv


-- | Iteration, encapsulating the type a -> b -> m (a, b, is)
data Iteration a b =
    Iteration
    {
        iteration :: a -> b -> MEnv' (a, b, IterationStack a b)
    }

-- | sequence of Iterations
type IterationStack a b =
    [Iteration a b]




--------------------------------------------------------------------------------
--  


makeIteration :: (a -> b -> MEnv' (a, b, IterationStack a b)) ->
                  Iteration a b
makeIteration = 
    Iteration




--------------------------------------------------------------------------------
--  ignore outer world b


type Iteration' a =
    Iteration a ()

type IterationStack' a =
    IterationStack a ()


iteration' :: Iteration' a -> a -> MEnv' (a, IterationStack' a)
iteration' iter = \a -> do
    (a', b, stack) <- (iteration iter) a ()
    return (a', stack)


makeIteration' :: (a -> MEnv' (a, IterationStack' a)) -> 
                 Iteration' a
makeIteration' f =
    makeIteration $ \a b -> fmap (\(a', stack) -> (a', b, stack)) (f a)




--------------------------------------------------------------------------------
--  ignore outer worlds b and a


type Iteration'' =
    Iteration' ()

type IterationStack'' =
    IterationStack' ()


iteration'' :: Iteration'' -> MEnv' (IterationStack'')
iteration'' iter = do
    (a', stack) <- (iteration' iter) ()
    return stack


makeIteration'' :: MEnv' (IterationStack'') -> Iteration''
makeIteration'' f =
    makeIteration' $ \a -> fmap (\stack -> (a, stack)) f





--------------------------------------------------------------------------------
--  defaultIteration


-- | the variable 's' makes it possible for iteration to work with a state 's'

--defaultIteration :: s -> 
--                    (s -> a -> b -> MEnv' ()) ->
--                    (s -> a -> b -> MEnv' (a, b, IterationStack a b)) -> 
--                    Iteration a b
--defaultIteration s output step = 
--    makeIteration $ \a b -> do
--        -- output
--        output s a b
--        
--        -- step
--        step s a b
--
defaultIteration :: s -> 
                    (s -> a -> b -> MEnv' (s, a, b)) ->
                    (s -> a -> b -> MEnv' (a, b, IterationStack a b)) -> 
                    Iteration a b
defaultIteration s output step = 
    makeIteration $ \a b -> do
        -- output
        (s', a', b') <- output s a b

        -- step
        step s' a' b'


--noOutput :: s -> a -> b -> MEnv' ()
--noOutput s a b =
--    return ()
noOutput :: s -> a -> b -> MEnv' (s, a, b)
noOutput = \s a b -> 
    return (s, a, b)



noStep :: s -> a -> b -> MEnv' (a, b, IterationStack a b)
noStep = \s a b -> 
    return (a, b, [])




--------------------------------------------------------------------------------
-- iterateWAStack 



-- | iterate a b using stack, returning (a', b', stack')
iterateABStack :: a -> b -> IterationStack a b -> MEnv' (a, b, IterationStack a b)
iterateABStack a b stack =
    case stack of
        []        -> return (a, b, [])
        (i:is)    -> do
            (a', b', top) <- (iteration i) a b
            return (a', b', top ++ is)


nullABStack :: (a, b, IterationStack a b) -> Bool
nullABStack (_, _, stack) =
    null stack


--------------------------------------------------------------------------------
--  Iteration Modifiers
--  note: we can probably add more such functions, like xxxW xxxA. but it seems that
--        it is more practical to control the flow by writing custom iterations, 
--        instead of using iteration modifiers. 


-- | modify world before iteration
modifyBefore :: Iteration a b ->
                (a -> b -> MEnv' (a, b)) -> 
                Iteration a b
modifyBefore iter modify =
    makeIteration $ curry $ uncurry modify >>> uncurry (iteration iter)


-- | modify world after iteration
modifyAfter :: Iteration a b -> 
               (a -> b -> MEnv' (a, b)) -> 
               Iteration a b
modifyAfter iter modify = 
    makeIteration $ \a b -> do
        (a', b', top) <- (iteration iter) a b
        case top of
            []      -> do
                (a'', b'') <- modify a' b'
                return (a'', b'', top)
            stack  -> do
                return (a', b', mapLast (\iter -> modifyAfter iter modify) stack)


-- | run iteration with local world, then continue with original world
localWorldA :: a -> 
               Iteration a b -> 
               Iteration a b
localWorldA world iter = 
    makeIteration $ \a b ->
        (iteration (modifyAfter iter (\_ b -> return (a, b)))) world b


-- | run iteration, then continue with original world
saveWorldA :: Iteration a b ->
              Iteration a b
saveWorldA iter =
    makeIteration $ \a b ->
        (iteration (localWorldA a iter)) a b


-- | after iteration, choose next iterations by looking at worlds
chooseTopAfter :: Iteration a b ->
                   (a -> b -> MEnv' (IterationStack a b)) ->
                   Iteration a b
chooseTopAfter iter f = 
    makeIteration $ \a b -> do 
        (a', b', top) <- (iteration iter) a b
        case top of
            []      -> do
                top' <- f a' b'
                return (a', b', top')
            top -> do
                return (a', b', mapLast (\iter -> chooseTopAfter iter f) top)



mapLast :: (a -> a) -> [a] -> [a]
mapLast f [] =
    []
mapLast f [a] =
    [f a]
mapLast f (a:as) =
    a : mapLast f as



