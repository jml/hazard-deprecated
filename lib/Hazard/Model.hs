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

module Hazard.Model (
  Hazard
  , addGame
  , getGameSlot
  , getGames
  , makeHazard
  , users
  , setGame
  , getRound
  ) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVar, modifyTVar, readTVar, writeTVar)
import Control.Error
import Control.Monad.Random
import Control.Monad.State

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


data Hazard = Hazard { games :: TVar [GameSlot Int]
                     , users :: UserDB
                     }


makeHazard :: STM Hazard
makeHazard = Hazard <$> newTVar [] <*> makeUserDB


getGames :: Hazard -> STM [GameSlot Int]
getGames = readTVar . games


getGameSlot :: Hazard -> Int -> STM (Maybe (GameSlot Int))
getGameSlot hazard i = do
  games' <- readTVar (games hazard)
  return $ atMay games' i


getRound :: Hazard -> Int -> Int -> STM (Maybe (Round Int))
getRound hazard i j =
  (fmap . (=<<)) (flip Games.getRound j . Games.gameState) (getGameSlot hazard i)


addGame :: Hazard -> GameSlot Int -> STM ()
addGame hazard game = do
  let allGames = games hazard
  games' <- readTVar allGames
  writeTVar allGames (games' ++ [game])


setGame :: Hazard -> Int -> GameSlot Int -> STM ()
setGame hazard i game =
  modifyTVar (games hazard) $
    \games' -> take i games' ++ [game] ++ drop (i+1) games'


type SlotResult a = Either (GameError Int) (a, GameSlot Int)


modifySlot :: Hazard -> Int -> (GameSlot Int -> SlotResult a) -> STM (SlotResult a)
modifySlot hazard i f = runEitherT $ do
  games' <- lift $ readTVar (games hazard)
  slot <- tryAt (GameNotFound i) games' i
  (a, slot') <- hoistEither $ f slot
  lift $ setGame hazard i slot'
  return (a, slot')


applySlotAction :: RandomGen g => Hazard -> Int -> g -> SlotAction g Int a -> STM (SlotResult a)
applySlotAction hazard i gen action = modifySlot hazard i (runSlotAction action gen)

applySlotAction' :: Hazard -> Int -> SlotAction' Int a -> STM (SlotResult a)
applySlotAction' hazard i action = modifySlot hazard i (runSlotAction' action)

performSlotAction :: Hazard -> Int -> SlotAction StdGen Int a -> IO (SlotResult a)
performSlotAction hazard i action = do
  gen <- newStdGen
  atomically $ applySlotAction hazard i gen action

performSlotAction' :: Hazard -> Int -> SlotAction' Int a -> IO (SlotResult a)
performSlotAction' hazard i = atomically . applySlotAction' hazard i
