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
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad.STM (atomically)

import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.Tasty
import Test.Tasty.Hspec

import Web.Scotty (scottyApp)

import Hazard (hazardWeb, makeHazard)

import qualified ModelTest


spec :: Spec
spec = with (atomically makeHazard >>= scottyApp . hazardWeb) $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200
  describe "/games" $ do
    it "GET returns empty list when there are no games" $ do
      get "/games" `shouldRespondWith` [json|[]|]
    it "POST creates game" $ do
      post "/games" [json|{numPlayers: 3, turnTimeout: 3600}|] `shouldRespondWith`
        "" {matchStatus = 201, matchHeaders = ["Location" <:> "/game/0"] }
    it "Created game appears in list" $ do
      post "/games" [json|{numPlayers: 3, turnTimeout: 3600}|]
      get "/games" `shouldRespondWith` [json|["/game/0"]|]
    it "Created game has POST data" $ do
      post "/games" [json|{numPlayers: 3, turnTimeout: 3600}|] `shouldRespondWith`
        "" {matchStatus = 201, matchHeaders = ["Location" <:> "/game/0"] }
      get "/game/0" `shouldRespondWith` [json|{numPlayers: 3, turnTimeout: 3600, creator: 0,
                                              state: "pending", players: [0]}|] {matchStatus = 200}


suite :: IO TestTree
suite = do
  spec' <- testSpec "Specification" spec
  return $ testGroup "Hazard" [ spec'
                              , ModelTest.suite
                              ]


main :: IO ()
main = do
  suite' <- suite
  defaultMain suite'
