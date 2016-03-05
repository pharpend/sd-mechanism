-- Snowdrift.coop - cooperative funding platform
-- Copyright (c) 2012-2016, Snowdrift.coop
-- 
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
-- 
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more
-- details.
-- 
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | 
-- Module      : Snowdrift.Mechanism.Types
-- Description : The non-database types for Snowdrift
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains types for the Snowdrift mechanism that are not stored in
-- SQLite databases. For said types, see "Snowdrift.Mechanism.Persistent"

module Snowdrift.Mechanism.Types where

class ToMechPatron a where
    mechPatron :: a -> Int
    toExternalPatron :: Int -> a

class ToMechProject a where
    mechProject :: a -> Int
    toExternalProject :: Int -> a

data MechError = InsufficientFunds | ExistingPledge | NoSuchProject | NoSuchPatron | ExistingPatron
    deriving (Show, Eq)
