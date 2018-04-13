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
module Game.Run.Eggs.SequenceEater
  (
    SequenceEater,
    makeSequenceEater,
    sequenceEat,

  ) where
  

import Data.List


data SequenceEater a =
    SequenceEater
    {
        eaterIdeal :: [a],
        eaterCurrent :: [a],
        eaterNexts :: [a]
    }
    
    
makeSequenceEater :: [a] -> SequenceEater a
makeSequenceEater ideal =
    SequenceEater
    {
        eaterIdeal = ideal,
        eaterCurrent = [],
        eaterNexts = ideal
    }


    
sequenceEat :: Eq a => SequenceEater a -> a -> (SequenceEater a, Bool)
sequenceEat eater a =
    let ideal = eaterIdeal eater
        current = eaterCurrent eater
        nexts = eaterNexts eater
    
    in case nexts of
        []      -> 
            let (current', nexts') = findIdeal ideal $ tail $ current ++ [a]
                eater' = eater { eaterCurrent = current',
                                 eaterNexts = ideal }
            in (eater', True)

            
        (n:ns)  -> if a == n
            then case ns of
                []  -> 
                    let (current', nexts') = findIdeal ideal $ tail $ current ++ [a]
                        eater' = eater { eaterCurrent = current',
                                         eaterNexts = ideal }
                    in (eater', True)
                   
                ns  ->
                    let current' = current ++ [a]
                        eater' = eater { eaterCurrent = current',
                                         eaterNexts = ns }
                    in (eater', False)
                    
            else
                let (current', nexts') = findIdeal ideal $ tail $ current ++ [a]
                    eater' = eater { eaterCurrent = current',
                                     eaterNexts = nexts' }
                in (eater', False)
                
                
                
                
findIdeal :: Eq a => [a] -> [a] -> ([a], [a])
findIdeal ideal seq =
    helper ideal $ tails seq
    where
      helper ideal (s:ss) =
          case maybeNexts ideal s of
              Just ns   -> (s, ns)
              Nothing    -> helper ideal ss
      helper ideal [] =
          ([], ideal)
          
          

maybeNexts :: Eq a => [a] -> [a] -> Maybe [a]
maybeNexts (i:is) (a:as) =
    if i == a then maybeNexts is as
              else Nothing
              
maybeNexts is [] =
    Just is
maybeNexts [] as =
    Nothing
    
