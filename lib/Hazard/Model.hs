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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hazard.Model (
  Hazard
  , addGame
  , getGameSlot
  , getGames
  , makeHazard
  , applySlotAction'
  , performSlotAction
  , performSlotAction'
  , runSlotAction'
  , users
  , getRound
  , tryGetSlot
  ) where

import BasicPrelude

import Control.Concurrent.STM (STM, TVar, atomically, newTVar, modifyTVar, readTVar, writeTVar)
import Control.Error hiding ((!?))
import Control.Monad.Random
import Data.Vector ((!?), (//))
import qualified Data.Vector as V

import Haverer.Round (Round)

import qualified Hazard.Games as Games
import Hazard.Games (
  GameSlot,
  GameError(..),
  SlotAction,
  SlotAction',
  runSlotAction,
  runSlotAction'
  )
import Hazard.Users (UserDB, makeUserDB)


data Hazard = Hazard { games :: TVar (Vector (GameSlot Int))
                     , users :: UserDB
                     }


makeHazard :: STM Hazard
makeHazard = Hazard <$> newTVar empty <*> makeUserDB


getGames :: Hazard -> STM (Vector (GameSlot Int))
getGames = readTVar . games


getGameSlot :: Hazard -> Int -> STM (Maybe (GameSlot Int))
getGameSlot hazard i = do
  games' <- readTVar (games hazard)
  return $ games' !? i


tryGetSlot :: Hazard -> Int -> EitherT (GameError e) STM (GameSlot Int)
tryGetSlot hazard i = do
  games' <- lift $ readTVar (games hazard)
  (games' !? i) ?? GameNotFound i


getRound :: Hazard -> Int -> Int -> STM (Maybe (Round Int))
getRound hazard i j =
  (fmap . (=<<)) (flip Games.getRound j . Games.gameState) (getGameSlot hazard i)


addGame :: Hazard -> GameSlot Int -> STM ()
addGame hazard game = do
  let allGames = games hazard
  games' <- readTVar allGames
  writeTVar allGames (V.snoc games' game)



type SlotResult e a = Either (GameError e) (a, GameSlot Int)


modifySlot :: Hazard -> Int -> (GameSlot Int -> SlotResult e a) -> STM (SlotResult e a)
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


applySlotAction :: RandomGen g => Hazard -> Int -> g -> SlotAction e g Int a -> STM (SlotResult e a)
applySlotAction hazard i gen action = modifySlot hazard i (runSlotAction action gen)

applySlotAction' :: Hazard -> Int -> SlotAction' e Int a -> STM (SlotResult e a)
applySlotAction' hazard i action = modifySlot hazard i (runSlotAction' action)

performSlotAction :: Hazard -> Int -> SlotAction e StdGen Int a -> IO (SlotResult e a)
performSlotAction hazard i action = do
  gen <- newStdGen
  atomically $ applySlotAction hazard i gen action

performSlotAction' :: Hazard -> Int -> SlotAction' e Int a -> IO (SlotResult e a)
performSlotAction' hazard i = atomically . applySlotAction' hazard i
