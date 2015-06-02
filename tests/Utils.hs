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

{-# LANGUAGE OverloadedStrings #-}

module Utils ( hazardTestApp
             , Utils.get
             , getAs
             , Utils.post
             , postAs
             , requiresAuth
             ) where

import Control.Monad.STM (atomically)

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import Data.Maybe

import Network.HTTP.Types.Header (Header)
import Network.Wai (Application)
import Test.Hspec.Wai
import Web.Spock.Safe (spockAsApp, spockT)

import Haverer.Deck
import Hazard (hazardWeb', makeHazard)


testDeck :: Deck Complete
testDeck = fromJust $ makeDeck [
  Soldier
  , Soldier
  , Clown
  , Minister
  , Wizard
  , Wizard
  , Soldier
  , Knight
  , Knight
  , Priestess
  , Clown
  , Soldier
  , General
  , Soldier
  , Priestess
  , Prince
  ]


hazardTestApp :: IO Application
hazardTestApp = do
  hazard <- atomically makeHazard
  spockAsApp $ spockT id $ hazardWeb' hazard (return "password") (return testDeck)


get url = request "GET" url [acceptJson] ""

post url = request "POST" url [acceptJson]

getAs username url = request "GET" url [authHeader username "password", acceptJson] ""

postAs username url = request "POST" url [authHeader username "password", acceptJson]


authHeader :: B.ByteString -> B.ByteString -> Header
authHeader username password = ("Authorization", B.concat ["Basic ", encodedCredentials])
  where encodedCredentials = Base64.encode $ B.concat [username, ":", password]


acceptJson :: Header
acceptJson = ("Accept", "application/json")


requiresAuth :: ResponseMatcher
requiresAuth = "Basic authentication is required" { matchStatus = 401
                                                  , matchHeaders = [
                                                    "WWW-Authenticate" <:> "Basic realm=\"Hazard API\""
                                                    ]
                                                  }
