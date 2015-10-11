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

module Hazard.GameAPI (GameAPI) where

import Servant
import Servant.HTML.Blaze (HTML)

import Hazard.Games (Game, GameID, RoundID)
import Hazard.Users (UserID)
import Haverer (
  Result,
  Round,
  )


-- XXX: Make this a thing.
data Auth

type GameAPI =
               "games"                           :> Get  '[JSON, HTML] [Game]
  :<|> Auth :> "games"                           :> Post '[JSON]       Game
  :<|>         "game" :> Capture "gameID" GameID :> Get  '[JSON, HTML] Game
  :<|> Auth :> "game" :> Capture "gameID" GameID :> Post '[JSON]       Game

  :<|>         "game" :> Capture "gameID" GameID :> "round" :> Capture "roundID" RoundID :> Get  '[JSON, HTML] (Round UserID)
  :<|> Auth :> "game" :> Capture "gameID" GameID :> "round" :> Capture "roundID" RoundID :> Post '[JSON]       (Result UserID)
