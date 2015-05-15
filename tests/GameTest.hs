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

module GameTest (suite) where

import Data.Aeson hiding (json)
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Network.Wai.Test (SResponse(..))
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.Tasty
import Test.Tasty.Hspec

import Utils (hazardTestApp, postAs, requiresAuth)


spec :: Spec
spec = with hazardTestApp $ do
  describe "GET /" $
    it "responds with 200" $
      get "/" `shouldRespondWith` 200

  describe "/games" $ do
    it "GET returns empty list when there are no games" $
      get "/games" `shouldRespondWith` [json|[]|]

    it "POST creates game" $ do
      post "/users" [json|{username: "foo"}|]
      postAs "foo" "/games" [json|{numPlayers: 3, turnTimeout: 3600}|] `shouldRespondWith`
        [json|null|] {matchStatus = 201, matchHeaders = ["Location" <:> "/game/0"] }

    it "unauthenticated POST fails" $
      post "/games" [json|{numPlayers: 3, turnTimeout: 3600}|] `shouldRespondWith` requiresAuth

    it "Created game appears in list" $ do
      post "/users" [json|{username: "foo"}|]
      postAs "foo" "/games" [json|{numPlayers: 3, turnTimeout: 3600}|]
      get "/games" `shouldRespondWith` [json|["/game/0"]|]

  describe "/game/N" $ do
    it "GET returns 404 if it hasn't been created" $
      get "/game/0" `shouldRespondWith` 404

    it "POST returns 404 if it hasn't been created" $ do
      post "/users" [json|{username: "foo"}|]
      postAs "foo" "/game/0" [json|null|] `shouldRespondWith` 404

    it "Created game has POST data" $ do
      game <- makeGameAs "foo" 3
      get game `shouldRespondWith` [json|{numPlayers: 3, turnTimeout: 3600, creator: 0,
                                          state: "pending", players: [0]}|] {matchStatus = 200}

    it "POST without authorization fails" $ do
      game <- makeGameAs "foo" 3
      post game [json|null|] `shouldRespondWith` requiresAuth

    it "Can be re-joined by same player" $ do
      game <- makeGameAs "foo" 3
      postAs "foo" game [json|null|] `shouldRespondWith`
        [json|{numPlayers: 3, turnTimeout: 3600, creator: 0,
               state: "pending", players: [0]}|] {matchStatus = 200}

    it "POST joins game" $ do
      game <- makeGameAs "foo" 3
      post "/users" [json|{username: "bar"}|]
      -- XXX: This tests the "order" that players appear in, when really I don't care.
      postAs "bar" game [json|null|] `shouldRespondWith`
        [json|{numPlayers: 3, turnTimeout: 3600, creator: 0,
               state: "pending", players: [1, 0]}|] {matchStatus = 200}

    it "POSTing to started game returns bad request" $ do
      game <- makeGameAs "foo" 2
      post "/users" [json|{username: "bar"}|]
      postAs "bar" game [json|null|]
      post "/users" [json|{username: "qux"}|]
      postAs "bar" game [json|null|] `shouldRespondWith`
        [json|{message: "Game already started"}|] {matchStatus = 400}

    it "Game starts when enough people have joined" $ do
      game <- makeGameAs "foo" 2
      post "/users" [json|{username: "bar"}|]
      postAs "bar" game [json|null|] `shouldRespondWith`
        [json|{numPlayers: 2, turnTimeout: 3600, creator: 0,
               state: "in-progress", players: [1, 0]}|] {matchStatus = 200}

    where
      makeGameAs :: Text -> Int -> WaiSession B.ByteString
      makeGameAs user numPlayers = do
        post "/users" (encode $ object ["username" .= (user :: Text)])
        response <- postAs (encodeUtf8 user) "/games" (encode $
                                                       object ["turnTimeout" .= (3600 :: Int)
                                                              ,"numPlayers" .= numPlayers])
        case lookup "Location" (simpleHeaders response) of
         Just path -> return path
         Nothing -> error "Did not create game: could not find return header"


suite :: IO TestTree
suite = testSpec "Game API" spec
