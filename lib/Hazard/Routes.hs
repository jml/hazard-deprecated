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

-- | Routes for Hazard API.
--
-- This module is intended to be imported qualified.

module Hazard.Routes (
  users,
  user,
  games,
  game,
  round
  ) where


import BasicPrelude hiding (round)
import Web.Spock.Safe


-- | Endpoint for managing users.
users :: Path '[]
users = static "users"


-- | Page for a single user.
user :: Path '[Int]
user = "user" <//> var


-- | Endpoint for creating and managing games.
games :: Path '[]
games = static "games"


-- | Page for a single game.
game :: Path '[Int]
game = "game" <//> var


-- | Page for a round in a game.
round :: Path '[Int, Int]
round = game <//> "round" <//> var
