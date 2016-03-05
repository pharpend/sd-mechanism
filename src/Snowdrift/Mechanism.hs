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
-- Module      : Snowdrift.Mechanism
-- Description : The mechanism for Snowdrift.coop.
-- Copyright   : Copyright (c) 2012-2016, Snowdrift.coop.
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX

module Snowdrift.Mechanism
        ( -- * Manage pledges etc.
          newPledge
        , newPatron
        , deletePledge
          -- * Statistics
        , patronPledges
          -- * Integration
        , migrateMech
        , ToMechPatron(..)
        , ToMechProject(..)
          -- ** Errors, nooo
        , MechError(..)
        ) where

import Control.Error
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Database.Persist
import Database.Persist.Sql (SqlBackend)

import Snowdrift.Mechanism.Persist
import Snowdrift.Mechanism.Types

-- *** Some conveniences (internal)
right :: Monad m => m a -> ExceptT e m a
right = ExceptT . fmap Right

type Mech a = forall m. MonadIO m => ReaderT SqlBackend m (Either MechError a)
type EMech a = forall m. MonadIO m => ExceptT MechError (ReaderT SqlBackend m) a

-- *** More internal stuff
-- | Convert user's value to a MechPatron
importPatron :: ToMechPatron a => a -> EMech (Entity MechPatron)
importPatron = (!? NoSuchPatron) . getBy . ExternalPatron . mechPatron

-- | Convert user's value to a MechProjectId
importProject :: ToMechProject a => a -> EMech (Entity MechProject)
importProject = (!? NoSuchProject) . getBy . ExternalProject . mechProject

-- *** Ahh, some external things.

-- | Register a new pledge. Currently throws an error if the pledge already
-- exists; maybe better to ignore it quietly?
newPledge :: (ToMechProject pro, ToMechPatron pat)
          => pro -> pat -> Mech ()
newPledge r a = runExceptT $ do
    Entity proId _   <- importProject r
    Entity patId pat <- importPatron a
    assertNonExistentPledge proId patId
    otherPatrons <- right $ count [PledgeProject ==. proId]
    case compare (mechPatronWallet pat) (3 * (1 + otherPatrons)) of
        LT -> throwE InsufficientFunds
        _  -> right $ insert_ (Pledge proId patId)
  where
    -- | Throw ExistingPledge if called for.
    assertNonExistentPledge pro pat = (!? ExistingPledge) $
        maybe (Just ()) (const Nothing) <$> getBy (UniquePledge pro pat)

newPatron :: ToMechPatron a => a -> Int -> Mech ()
newPatron a bal = runExceptT $ do
    let k = mechPatron a
    void $ insertUnique (MechPatron bal k) !? ExistingPatron

-- | Delete a pledge. Currently ignores nonexistent pledges quietly,
-- because that's the Persistent default for deleteBy, but complains about
-- nonexistent patrons and projects. Maybe it should ignore their absence
-- quietly?
deletePledge :: (ToMechProject pro, ToMechPatron pat)
             => pro -> pat -> Mech ()
deletePledge r a = runExceptT $ do
    pro <- entityKey <$> importProject r
    pat <- entityKey <$> importPatron a
    right (deleteBy (UniquePledge pro pat))

-- | Get a list of patron pledges. Or, get an error, if the patron doesn't
-- exist.
patronPledges :: (ToMechProject pro, ToMechPatron pat)
              => pat -> Mech [pro]
patronPledges a = runExceptT $ do
    pat <- entityKey <$> importPatron a
    right $ do
        pros <- fmap (map (pledgeProject . entityVal))
                     (selectList [PledgePatron ==. pat] [])
        fmap (map exportProject)
             (selectList [MechProjectId <-. pros] [])
  where
    exportProject = toExternalProject . mechProjectExternalKey . entityVal
