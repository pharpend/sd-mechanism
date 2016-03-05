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
-- Module      : Snowdrift.Mechanism.Perist
-- Description : Persistent datatype declarations for the mechanism
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX
-- 
-- <http://www.yesodweb.com/book/persistent Persistent> is a Haskell library to
-- interact with SQL databases. This module exists to call some peristent
-- functions to interact with SQL databases on the host machine.


module Snowdrift.Mechanism.Persist where

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateMech"] [persistLowerCase|
MechPatron
    wallet Int
    externalKey Int
    ExternalPatron externalKey
MechProject
    dropbox Int
    externalKey Int
    ExternalProject externalKey
Pledge
    project MechProjectId
    patron MechPatronId
    UniquePledge project patron
|]
