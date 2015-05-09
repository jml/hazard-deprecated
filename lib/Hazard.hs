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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Hazard ( Hazard
              , hazardWeb
              , makeHazard
              ) where


import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (STM, atomically)
import Control.Concurrent.STM (TVar, newTVar, readTVar, writeTVar)

import Data.Aeson (FromJSON(..), ToJSON(..), (.=), Value(Object), object, (.:))
import Network.HTTP.Types.Status
import Web.Scotty

import Hazard.Model (
  createGame,
  GameCreationRequest(..),
  GameSlot,
  requestGame,
  Validated(Unchecked),
  validateCreationRequest)


instance ToJSON (GameCreationRequest a) where
  toJSON r = object [ "numPlayers" .= reqNumPlayers r
                    , "turnTimeout" .= reqTurnTimeout r
                    ]

instance FromJSON (GameCreationRequest 'Unchecked) where
  parseJSON (Object v) = requestGame <$> v .: "numPlayers" <*> v .: "turnTimeout"
  parseJSON _ = mzero


data Hazard = Hazard (TVar [GameSlot Int])


makeHazard :: STM Hazard
makeHazard = Hazard <$> newTVar []


getGames :: Hazard -> STM [GameSlot Int]
getGames (Hazard allGames) = readTVar allGames

getGame :: Hazard -> Int -> STM (Maybe (GameSlot Int))
getGame (Hazard allGames) i = do
  games <- readTVar allGames
  if 0 <= i && i < length games
    then return $ Just (games !! i)
    else return Nothing

addGame :: Hazard -> GameSlot Int -> STM ()
addGame (Hazard allGames) game = do
  games <- readTVar allGames
  writeTVar allGames (games ++ [game])


hazardWeb :: Hazard -> ScottyM ()
hazardWeb hazard = do
  get "/" $ html "Hello World!"
  get "/games" $ do
    games <- liftIO $ atomically $ getGames hazard
    json ["/game/" ++ show i | i <- [0..length games - 1]]
  post "/games" $ do
    gameRequest <- jsonData :: ActionM (GameCreationRequest 'Unchecked)
    case validateCreationRequest gameRequest of
     Left e -> error $ show e  -- XXX: Should be bad request
     Right r -> do
       let newGame = createGame 0 r
       liftIO $ atomically $ addGame hazard newGame
       status created201
       setHeader "Location" "/game/0"
       raw ""
  get "/game/:id" $ do
    gameId <- param "id"
    game <- liftIO $ atomically $ getGame hazard gameId
    json game
