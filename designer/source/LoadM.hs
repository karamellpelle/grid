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
module LoadM
  (
    LoadM,
    tryLoading,
    logging,
    loggingWarning,
    loggingError,

    module Control.Monad,
    module Control.Monad.Trans,
  ) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Data.Either()
import System.IO



type LoadM =
    ErrorT String IO 


-- | try to load an object with a LoadM monad
tryLoading :: LoadM a -> IO (Maybe a)
tryLoading la = do
    res <- runErrorT la
    return $ either (const Nothing) Just res


-- | log message to stderr 
logging :: String -> LoadM ()
logging str = do
    liftIO $ hPutStrLn stderr str


-- | log a warning to stderr
loggingWarning :: String -> LoadM ()
loggingWarning str = do
    logging $ "\nwarning: " ++ str


-- | log an error to stderr, and escape from LoadM monad
loggingError :: String -> LoadM ()
loggingError str = do
    logging $ "\nerror:   " ++ str
    throwError str




