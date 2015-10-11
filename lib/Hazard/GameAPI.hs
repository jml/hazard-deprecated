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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Hazard.GameAPI (GameAPI, gameAPI, server) where

import BasicPrelude

import Control.Concurrent.STM (atomically)
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Either (EitherT)
import Data.Vector (toList)

import Servant

import Hazard.Games (GameSlot, GameID, RoundID)
import Hazard.Model (Hazard, getGames, getGameSlot)
import Hazard.Users (UserID)
import Haverer (Round)


-- XXX: Make this a thing.
-- data Auth

type GameAPI =
               "games"                           :> Get  '[JSON] [GameSlot]
--  :<|> Auth :> "games"                           :> Post '[JSON]       Game
  :<|>         "game" :> Capture "gameID" GameID :> Get  '[JSON] GameSlot
--  :<|> Auth :> "game" :> Capture "gameID" GameID :> Post '[JSON]       Game

  :<|>         "game" :> Capture "gameID" GameID :> "round" :> Capture "roundID" RoundID :> Get  '[JSON] (Round UserID)
--  :<|> Auth :> "game" :> Capture "gameID" GameID :> "round" :> Capture "roundID" RoundID :> Post '[JSON]       (Result UserID)


gameAPI :: Proxy GameAPI
gameAPI = Proxy


type GameHandler = EitherT ServantErr IO

server :: Hazard -> Server GameAPI
server hazard = getAllGames hazard :<|> getGame hazard :<|> getRound

getAllGames :: Hazard -> GameHandler [GameSlot]
getAllGames = map toList . liftIO . atomically . getGames

getGame :: Hazard -> GameID -> GameHandler GameSlot
getGame hazard gameId = do
  game <- liftIO $ atomically $ getGameSlot hazard gameId
  case game of
    Just game' -> return game'
    Nothing -> throwError $ err404 { errBody = "no such game" }


getRound :: GameID -> RoundID -> GameHandler (Round UserID)
getRound = undefined