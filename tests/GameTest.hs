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

import Control.Monad (unless)
import Data.Aeson hiding (json)
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Network.Wai.Test (SResponse(..))
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.Tasty
import Test.Tasty.Hspec

import Haverer.Deck (Card)

import Utils (getAs, hazardTestApp, postAs, requiresAuth)


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
        [json|{numPlayers: 2,
               turnTimeout: 3600,
               creator: 0,
               state: "in-progress",
               players: [1, 0],
               scores: [0, 0]}|] {matchStatus = 200}


  describe "Playing a game" $ do
    it "Rounds don't exist for unstarted game" $ do
      game <- makeGameAs "foo" 2
      get (B.concat [game, "/round/0"]) `shouldRespondWith` 404

    it "started game is started" $ do
      (game, _) <- makeStartedGame 3
      get game `shouldRespondWith` [json|{
                                       numPlayers: 3,
                                       turnTimeout: 3600,
                                       creator: 0,
                                       state: "in-progress",
                                       players: [2,1,0],
                                       scores: [0,0,0]
                                       }|] {matchStatus = 200}

    it "Rounds do exist for started games" $ do
      -- XXX: This warning here means that we'e only ever dealing with the
      -- integer player IDs, and never the user names. I don't know whether
      -- that's a good thing or a bad thing, so I'm going to leave it as
      -- integers until we start implementing actual clients.
      (game, [foo, bar, baz]) <- makeStartedGame 3
      get (B.concat [game, "/round/0"]) `hasJsonResponse`
        object [
          "players" .= [
             object [
                "id" .= (0 :: Int),
                "active" .= True,
                "protected" .= False,
                "discards" .= ([] :: [Int])
                ],
             object [
               "id" .= (1 :: Int),
               "active" .= True,
               "protected" .= False,
               "discards" .= ([] :: [Int])
               ],
             object [
               "id" .= (2 :: Int),
               "active" .= True,
               "protected" .= False,
               "discards" .= ([] :: [Int])
               ]
             ],
          -- XXX: *Actually* the first player should be randomized. Currently
          -- it's always the last person who signed up.
          "currentPlayer" .= (2 :: Int)
          ]

    it "Shows your hand when you GET" $ do
      (game, [_, bar, _]) <- makeStartedGame 3
      response <- getAs (encodeUtf8 bar) (B.concat [game, "/round/0"])
      jsonResponseIs response (isJust . getJsonHand 1) True

    it "Doesn't show other hands when you GET" $ do
      (game, [_, bar, _]) <- makeStartedGame 3
      response <- getAs (encodeUtf8 bar) (B.concat [game, "/round/0"])
      jsonResponseIs response (isJust . getJsonHand 0) False
      jsonResponseIs response (isJust . getJsonHand 2) False

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

    makeStartedGame :: Int -> WaiSession (B.ByteString, [Text])
    makeStartedGame n =
      let userPool = ["foo", "bar", "baz", "qux"]
          users = take n userPool
          creator:others = users
      in do
        game <- makeGameAs creator n
        for_ others (\u -> do post "/users" (encode $ object ["username" .= u])
                              postAs (encodeUtf8 u) game [json|null|] `shouldRespondWith` 200)
        return (game, users)

    getJsonHand :: Int -> Value -> Maybe Card
    getJsonHand i = parseMaybe (withObject "expected round object" (
                                   \obj -> do players <- obj .: "players"
                                              (players !! i) .: "hand"))


    jsonResponseIs :: (FromJSON a, Eq b, Show b) => SResponse -> (a -> b) -> b -> WaiSession ()
    jsonResponseIs response f value =
      unless ((f <$> decode body) == Just value)
        (liftIO $ expectationFailure $ unlines [
            "Expected: " ++ show value,
            "Actual: " ++ lazyToString body
            ])
      where body = simpleBody response

    hasJsonResponse :: (Eq a, Show a, FromJSON a, ToJSON a) => WaiSession SResponse -> a -> WaiSession ()
    hasJsonResponse response x = do
      response' <- response
      jsonResponseIs response' id x


-- XXX: Is there an easier way?
lazyToString :: L.ByteString -> String
lazyToString = Text.unpack . decodeUtf8 . L.toStrict


suite :: IO TestTree
suite = testSpec "Game API" spec
