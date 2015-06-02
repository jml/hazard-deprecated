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

module Utils ( hazardTestApp
             , hazardTestApp'
             , Utils.get
             , getAs
             , Utils.post
             , postAs
             , requiresAuth
             , makeTestDeck
             ) where

import BasicPrelude

import Control.Monad.STM (atomically)

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text
import Data.IORef
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



makeTestApp :: IO ByteString -> IO (Deck Complete) -> IO Application
makeTestApp passwords decks = do
  hazard <- atomically makeHazard
  spockAsApp $ spockT id $ hazardWeb' hazard passwords decks


hazardTestApp :: IO Application
hazardTestApp = makeTestApp  (return "password") (return testDeck)

hazardTestApp' :: IORef (Deck Complete) -> IO Application
hazardTestApp' decks = makeTestApp  (return "password") (readIORef decks)


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


makeTestDeck :: Text -> Deck Complete
makeTestDeck =
  fromJust . makeDeck . map charToCard . textToString . Text.toLower
  where
    charToCard 's' = Soldier
    charToCard 'c' = Clown
    charToCard 'k' = Knight
    charToCard 'p' = Priestess
    charToCard 'w' = Wizard
    charToCard 'g' = General
    charToCard 'm' = Minister
    charToCard 'x' = Prince
