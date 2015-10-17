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

import BasicPrelude hiding (round)

import Control.Concurrent.STM (atomically)
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Either (EitherT, bimapEitherT, hoistEither)
import qualified Data.ByteString.Lazy as LazyBytes
import Data.Vector (toList)

import Servant

import Hazard.HttpAuth (Credentials(..), PasswordAuth, PasswordProtected, protectWith)

import Hazard.Games (GameSlot, GameID, RoundID, GameCreationRequest, Validated(..), validateCreationRequest)
import Hazard.Model (Hazard, createGame, getGames, getGameSlot, getRound, users)
import Hazard.Users (User, UserID, authenticate, getUserID)
import Haverer (Round)


-- XXX: Nothing to do with Game. General authentication API.
type Auth = PasswordAuth User

type AuthProtected = PasswordProtected User

auth :: Hazard -> handlers -> AuthProtected handlers
auth hazard = protectWith "Hazard" checkPassword
  where
    checkPassword creds =
      liftIO $ atomically $ authenticate (users hazard) (username creds) (password creds)
-- End authentication API


type GameAPI =
               "games"                                                     :> Get  '[JSON] [GameSlot]
  :<|> Auth :> "games" :> ReqBody '[JSON] (GameCreationRequest 'Unchecked) :> Post '[JSON] GameSlot
  :<|>         "game" :> Capture "gameID" GameID                           :> Get  '[JSON] GameSlot
--  :<|> Auth :> "game" :> Capture "gameID" GameID :> Post '[JSON]       Game

  :<|>         "game" :> Capture "gameID" GameID :> "round" :> Capture "roundID" RoundID :> Get  '[JSON] (Round UserID)
--  :<|> Auth :> "game" :> Capture "gameID" GameID :> "round" :> Capture "roundID" RoundID :> Post '[JSON]       (Result UserID)


gameAPI :: Proxy GameAPI
gameAPI = Proxy


type GameHandler = EitherT ServantErr IO

server :: Hazard -> Server GameAPI
server hazard = getAllGames hazard :<|> createOneGame hazard :<|> getGame hazard :<|> getOneRound hazard


getAllGames :: Hazard -> GameHandler [GameSlot]
getAllGames = map toList . liftIO . atomically . getGames


createOneGame :: Hazard -> AuthProtected (User -> GameCreationRequest 'Unchecked -> GameHandler GameSlot)
createOneGame hazard = auth hazard (createGame' hazard)


createGame' :: Hazard -> User -> GameCreationRequest 'Unchecked -> GameHandler GameSlot
createGame' hazard user request = do
  req <- bimapEitherT toServantErr id (hoistEither (validateCreationRequest request))
  (_gameID, game) <- liftIO $ atomically $ createGame hazard (getUserID user) req
  return game
  where
    toServantErr e = err400 { errBody = LazyBytes.fromStrict (encodeUtf8 (show e)) }


getGame :: Hazard -> GameID -> GameHandler GameSlot
getGame hazard gameId = do
  game <- liftIO $ atomically $ getGameSlot hazard gameId
  case game of
    Just game' -> return game'
    Nothing -> throwError $ err404 { errBody = "no such game" }


getOneRound :: Hazard -> GameID -> RoundID -> GameHandler (Round UserID)
getOneRound hazard gameId roundId = do
  round <- liftIO $ atomically $ getRound hazard gameId roundId
  case round of
    Nothing -> throwError $ err404 { errBody = "no such round" }
    Just round' -> return round'
