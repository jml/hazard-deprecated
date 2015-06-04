-- Copyright (c) 2015 Jonathan M. Lange <jml@mumak.net>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hazard.Model (
  Hazard
  , createGame
  , getGameSlot
  , getGames
  , makeHazard
  , applySlotAction
  , runSlotAction
  , users
  , getRound
  , tryGetSlot
  ) where

import BasicPrelude

import Control.Concurrent.STM (STM, TVar, newTVar, modifyTVar, readTVar, writeTVar)
import Control.Error hiding ((!?))
import Data.Vector ((!?), (//))
import qualified Data.Vector as V

import Haverer.Round (Round)

import qualified Hazard.Games as Games
import Hazard.Games (
  GameID,
  GameCreationRequest(..),
  GameSlot,
  GameError(..),
  Validated(..),
  SlotAction,
  runSlotAction
  )
import Hazard.Users (UserDB, UserID, makeUserDB)


data Hazard = Hazard { games :: TVar (Vector GameSlot)
                     , users :: UserDB
                     }


type RoundID = Int


makeHazard :: STM Hazard
makeHazard = Hazard <$> newTVar empty <*> makeUserDB


getGames :: Hazard -> STM (Vector GameSlot)
getGames = readTVar . games


getGameSlot :: Hazard -> GameID -> STM (Maybe GameSlot)
getGameSlot hazard i = do
  games' <- readTVar (games hazard)
  return $ games' !? i


tryGetSlot :: Hazard -> GameID -> EitherT (GameError e) STM GameSlot
tryGetSlot hazard i = do
  games' <- lift $ readTVar (games hazard)
  (games' !? i) ?? GameNotFound i


getRound :: Hazard -> GameID -> RoundID -> STM (Maybe (Round UserID))
getRound hazard i j =
  (fmap . (=<<)) (flip Games.getRound j . Games.gameState) (getGameSlot hazard i)


createGame :: Hazard -> UserID -> GameCreationRequest 'Valid -> STM (GameID, GameSlot)
createGame hazard creator request = do
  let allGames = games hazard
  games' <- readTVar allGames
  writeTVar allGames (V.snoc games' game)
  return (length games', game)
  where
    game = Games.createGame creator request


type SlotResult e a = Either (GameError e) (a, GameSlot)


modifySlot :: Hazard -> GameID -> (GameSlot -> SlotResult e a) -> STM (SlotResult e a)
modifySlot hazard i f = runEitherT $ do
  slot <- tryGetSlot hazard i
  (a, slot') <- hoistEither $ f slot
  lift $ setGame slot'
  return (a, slot')
  where
    slotVar = games hazard
    setGame game =
      modifyTVar slotVar $
      \games' -> games' // [(i, game)]


applySlotAction :: Hazard -> GameID -> SlotAction e a -> STM (SlotResult e a)
applySlotAction hazard i action = modifySlot hazard i (runSlotAction action)
