-- Copyright (c) 2014-2015 Jonathan M. Lange <jml@mumak.net>
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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicPrelude

import Control.Concurrent.STM (atomically)
import Network.Wai.Handler.Warp (run)
import Servant

import qualified Hazard.GameAPI as GameAPI
import qualified Hazard.UserAPI as UserAPI
import Hazard.Users (UserDB, makeUserDB, makePassword)


type API = UserAPI.UserAPI :<|> GameAPI.GameAPI

api :: Proxy API
api = Proxy


server :: UserDB -> UserAPI.PasswordGenerator -> Server API
server userDB pwgen = UserAPI.server userDB pwgen :<|> GameAPI.server


main :: IO ()
main = do
  userDB <- atomically makeUserDB
  run 8080 $ serve api $ server userDB makePassword
