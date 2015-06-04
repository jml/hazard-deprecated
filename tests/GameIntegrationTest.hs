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
{-# LANGUAGE QuasiQuotes #-}

module GameIntegrationTest (suite) where

import BasicPrelude

import Data.Aeson hiding (json)
import Data.Aeson.Types (parseMaybe)
import Data.Foldable (for_)
import Data.IORef
import Data.Maybe (fromJust)

import Network.Wai.Test (SResponse(..), assertStatus, assertContentType)
import Test.Hspec.Wai hiding (get, post)
import Test.Hspec.Wai.Internal (WaiSession(WaiSession))
import Test.Hspec.Wai.JSON
import Test.Tasty
import Test.Tasty.Hspec

import Haverer.Deck (Card(..), Complete, Deck, makeDeck)

import Utils (get, getAs, hazardTestApp', post, postAs, requiresAuth, makeTestDeck)


testDeck :: Deck Complete
testDeck = makeTestDeck "sscmwwskkpcsgspx"



spec :: IORef (Deck Complete) -> Spec
spec deckVar = with (hazardTestApp' deckVar) $ do
  describe "GET /" $
    it "responds with 200" $
      get "/" `shouldRespondWith` 200

  describe "/games" $ do
    it "GET returns empty list when there are no games" $
      get "/games" `shouldRespondWith` [json|[]|]

    it "POST creates game" $ do
      post "/users" [json|{username: "foo"}|]
      postAs "foo" "/games" [json|{numPlayers: 3, turnTimeout: 3600}|] `shouldRespondWith`
        [json|{"creator": "0", "state": "pending", "players": ["0"], "turnTimeout": 3600,
              "numPlayers": 3}|]
        {matchStatus = 201, matchHeaders = ["Location" <:> "/game/0"] }

    it "POST twice creates 2 game" $ do
      post "/users" [json|{username: "foo"}|]
      postAs "foo" "/games" [json|{numPlayers: 3, turnTimeout: 3600}|] `shouldRespondWith`
        [json|{"creator": "0", "state": "pending", "players": ["0"], "turnTimeout": 3600,
              "numPlayers": 3}|]
        {matchStatus = 201, matchHeaders = ["Location" <:> "/game/0"] }
      postAs "foo" "/games" [json|{numPlayers: 2, turnTimeout: 3600}|] `shouldRespondWith`
        [json|{"creator": "0", "state": "pending", "players": ["0"], "turnTimeout": 3600,
              "numPlayers": 2}|] {matchStatus = 201, matchHeaders = ["Location" <:> "/game/1"] }

    it "URLs from POSTs align properly" $ do
      -- Post a 2 player game and 3 player game, and make sure that when we
      -- GET the URLs that number of players is as we requested.
      post "/users" [json|{username: "foo"}|]
      postAs "foo" "/games" [json|{numPlayers: 3, turnTimeout: 3600}|]
      postAs "foo" "/games" [json|{numPlayers: 2, turnTimeout: 3600}|]
      game0 <- get "/game/0"
      jsonResponseIs game0 (getKey "numPlayers") (Just 3 :: Maybe Int)
      game1 <- get "/game/1"
      jsonResponseIs game1 (getKey "numPlayers") (Just 2 :: Maybe Int)

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
      get game `shouldRespondWith` [json|{numPlayers: 3, turnTimeout: 3600, creator: "0",
                                          state: "pending", players: ["0"]}|] {matchStatus = 200}

    it "POST without authorization fails" $ do
      game <- makeGameAs "foo" 3
      post game [json|null|] `shouldRespondWith` requiresAuth

    it "Can be re-joined by same player" $ do
      game <- makeGameAs "foo" 3
      postAs "foo" game [json|null|] `shouldRespondWith`
        [json|{numPlayers: 3, turnTimeout: 3600, creator: "0",
               state: "pending", players: ["0"]}|] {matchStatus = 200}

    it "POST joins game" $ do
      game <- makeGameAs "foo" 3
      post "/users" [json|{username: "bar"}|]
      -- XXX: This tests the "order" that players appear in, when really I don't care.
      postAs "bar" game [json|null|] `shouldRespondWith`
        [json|{numPlayers: 3, turnTimeout: 3600, creator: "0",
               state: "pending", players: ["1", "0"]}|] {matchStatus = 200}

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
               creator: "0",
               state: "in-progress",
               players: ["1", "0"],
               scores: [0, 0]}|] {matchStatus = 200}

    it "Same data on GET after POST" $ do
      game <- makeGameAs "foo" 2
      post "/users" [json|{username: "bar"}|]
      postAs "bar" game [json|null|]
      get game `shouldRespondWith`
        [json|{numPlayers: 2,
               turnTimeout: 3600,
               creator: "0",
               state: "in-progress",
               players: ["1", "0"],
               scores: [0, 0]}|] {matchStatus = 200}


  describe "Playing a game" $ do
    it "Rounds don't exist for unstarted game (GET)" $ do
      game <- makeGameAs "foo" 2
      get (game ++ "/round/0") `shouldRespondWith` 404

    it "Rounds don't exist for unstarted game (POST)" $ do
      game <- makeGameAs "foo" 2
      postAs "foo" (game ++ "/round/0") [json|{card: "priestess"}|] `shouldRespondWith` 404

    it "started game is started" $ do
      (game, _) <- makeStartedGame 3
      get game `shouldRespondWith` [json|{
                                       numPlayers: 3,
                                       turnTimeout: 3600,
                                       creator: "0",
                                       state: "in-progress",
                                       players: ["2","1","0"],
                                       scores: [0,0,0]
                                       }|] {matchStatus = 200}

    it "Rounds do exist for started games" $ do
      -- XXX: This warning here means that we'e only ever dealing with the
      -- integer player IDs, and never the user names. I don't know whether
      -- that's a good thing or a bad thing, so I'm going to leave it as
      -- integers until we start implementing actual clients.
      let deck = fromJust $ makeDeck [Soldier,Soldier,Clown,Minister,Wizard,Wizard,Soldier,Knight,Knight,Priestess,Clown,Soldier,General,Soldier,Priestess,Prince]
      (game, [foo, bar, baz]) <- makeStartedGame' 3 deck
      get (game ++ "/round/0") `hasJsonResponse`
        object [
          "players" .= [
             object [
                "id" .= ("0" :: Text),
                "active" .= True,
                "protected" .= False,
                "discards" .= ([] :: [Card])
                ],
             object [
               "id" .= ("1" :: Text),
               "active" .= True,
               "protected" .= False,
               "discards" .= ([] :: [Card])
               ],
             object [
               "id" .= ("2" :: Text),
               "active" .= True,
               "protected" .= False,
               "discards" .= ([] :: [Card])
               ]
             ],
          -- XXX: *Actually* the first player should be randomized. Currently
          -- it's always the last person who signed up.
          "currentPlayer" .= ("2" :: Text)
          ]

    it "Shows your hand when you GET" $ do
      (game, [_, bar, _]) <- makeStartedGame 3
      response <- getAs (encodeUtf8 bar) (game ++ "/round/0")
      jsonResponseIs response (isJust . (getPlayer 1 >=> getCard "hand")) True

    it "Doesn't show other hands when you GET" $ do
      (game, [_, bar, _]) <- makeStartedGame 3
      response <- getAs (encodeUtf8 bar) (game ++ "/round/0")
      jsonResponseIs response (getPlayer 0 >=> getKey "hand") (Nothing :: Maybe Card)
      jsonResponseIs response (getPlayer 2 >=> getKey "hand") (Nothing :: Maybe Card)

    it "Doesn't include dealt card when it's not your turn" $ do
      (game, [_, bar, _]) <- makeStartedGame 3
      response <- getAs (encodeUtf8 bar) (game ++ "/round/0")
      jsonResponseIs response (getKey "dealtCard") (Nothing :: Maybe Card)

    it "Does include dealt card when it is your turn" $ do
      (game, [_, _, baz]) <- makeStartedGame 3
      response <- getAs (encodeUtf8 baz) (game ++ "/round/0")
      jsonResponseIs response (isJust . getCard "dealtCard") True

    it "POST without authorization fails" $ do
      (game, _) <- makeStartedGame 3
      post (game ++ "/round/0") [json|null|] `shouldRespondWith` requiresAuth

    it "POST when it's not your turn returns error" $ do
      (game, [foo, _, _]) <- makeStartedGame 3
      postAs (encodeUtf8 foo) (game ++ "/round/0") [json|{card: "priestess"}|]
        `shouldRespondWith` [json|{message: "Not your turn",
                                   currentPlayer: "2"}|] { matchStatus = 400 }

    it "POST when you aren't in the game returns error" $ do
      (game, _) <- makeStartedGame 3
      post "/users" [json|{username: "qux"}|]
      postAs "qux" (game ++ "/round/0") [json|{card: "priestess"}|]
        `shouldRespondWith` [json|{message: "You are not playing"}|] { matchStatus = 400 }

    it "POST does *something*" $ do
      let deck = makeTestDeck "sscmwwskkpcsgspx"
      (game, [_, _, baz]) <- makeStartedGame' 3 deck
      -- Hands are Soldier, Clown, Minister. Player is dealt Wizard.

      -- Play Wizard on self.
      let roundUrl = game ++ "/round/0"
          user = encodeUtf8 baz
      postAs user roundUrl [json|{card: "wizard", target: "2"}|]
        `shouldRespondWith` [json|{id: "2", result: "forced-discard", card: "Wizard", target: "2"}|]

    it "ending round reports winner correctly" $ do
      -- XXX: Deals cards, then burns, then draws. Really should be burn, deal draw.
      -- XXX: Players are 0, 1. Player 1 "goes" first, and is dealt the first
      -- card from the deck.
      let deck = makeTestDeck "skcmwwskspcsgspx"
      (game, [_, bar]) <- makeStartedGame' 2 deck
      let roundUrl = game ++ "/round/0"
          user = encodeUtf8 bar
      postAs user roundUrl [json|{card: "soldier", target: "0", guess: "knight"}|]
        `shouldRespondWith`
        [json|{id: "1", result: "eliminated", card: "Soldier", guess: "Knight", target: "0", eliminated: "0"}|]
        { matchStatus = 200 }
      -- XXX: burnt card
      -- XXX: survivors

      -- XXX: Maybe the testing strategy here should be to test various
      -- serializations of Round, and use the Haverer testing library to
      -- generate rounds in the states we find interesting?
      getAs user roundUrl `shouldRespondWith`
        [json|
         {currentPlayer: null,
          winners: ["1"],
          players: [
            {protected:null,
             active:false,
             id:"0",
             discards:["Knight"]
            },
            {protected:false,
             active:true,
             id:"1",
             hand:"Minister",
             discards:["Soldier"]
            }
            ]}|] { matchStatus = 200 }

    it "cannot POST to round after round is over" $ do
      let deck = makeTestDeck "skcmwwskspcsgspx"
      (game, [_, bar]) <- makeStartedGame' 2 deck
      let roundUrl = game ++ "/round/0"
          user = encodeUtf8 bar
      postAs user roundUrl [json|{card: "soldier", target: "0", guess: "knight"}|]
      postAs user roundUrl [json|{card: "soldier", target: "0", guess: "knight"}|]
        `shouldRespondWith`
        [json|{message: "Round not active"}|] { matchStatus = 400 }

    it "updates game when round is over" $ do
      let deck = makeTestDeck "skcmwwskspcsgspx"
      (game, [_, bar]) <- makeStartedGame' 2 deck
      let roundUrl = game ++ "/round/0"
          user = encodeUtf8 bar
      postAs user roundUrl [json|{card: "soldier", target: "0", guess: "knight"}|]
      get game
      `shouldRespondWith`
        -- XXX: This is confusing. players & scores should just be one dict.
        [json|{"creator":"0","state":"in-progress","players":["1","0"],"scores":[0,1],"turnTimeout":3600,"numPlayers":2}|]
        { matchStatus = 200 }

    it "creates new round after previous is over" $ do
      let deck = makeTestDeck "skcmwwskspcsgspx"
      (game, [_, bar]) <- makeStartedGame' 2 deck
      let roundUrl = game ++ "/round/0"
          user = encodeUtf8 bar
      postAs user roundUrl [json|{card: "soldier", target: "0", guess: "knight"}|]
      let roundUrl2 = game ++ "/round/1"
      -- XXX: Should just GET here, rather than POST
      postAs user roundUrl2 [json|{card: "soldier", target: "0", guess: "knight"}|]
        `shouldRespondWith`
        [json|{id: "1", result: "eliminated", card: "Soldier", guess: "Knight", target: "0", eliminated: "0"}|]
        { matchStatus = 200 }

      -- TODO: Test that there's interesting history available at round/0

  where
    makeGameAs :: Text -> Int -> WaiSession ByteString
    makeGameAs user numPlayers = do
      post "/users" (encode $ object ["username" .= (user :: Text)])
      response <- postAs (encodeUtf8 user) "/games" (encode $
                                                     object ["turnTimeout" .= (3600 :: Int)
                                                            ,"numPlayers" .= numPlayers])
      case lookup "Location" (simpleHeaders response) of
       Just path -> return path
       Nothing -> error "Did not create game: could not find return header"


    makeStartedGame :: Int -> WaiSession (ByteString, [Text])
    makeStartedGame n = makeStartedGame' n testDeck

    makeStartedGame' :: Int -> Deck Complete -> WaiSession (ByteString, [Text])
    makeStartedGame' n deck =
      let userPool = ["foo", "bar", "baz", "qux"]
          users = take n userPool
          creator:others = users
      in do
        liftIO $ writeIORef deckVar deck
        game <- makeGameAs creator n
        for_ others (\u -> do post "/users" (encode $ object ["username" .= u])
                              postAs (encodeUtf8 u) game [json|null|] `shouldRespondWith` 200)
        return (game, users)

    getPlayer i v = getKey "players" v >>= \ps -> ps !! i


    getKey :: FromJSON a => Text -> Value -> Maybe a
    getKey key = parseMaybe (withObject "expected object" (.: key))

    getCard :: Text -> Value -> Maybe Card
    getCard = getKey

    getJsonResponse :: FromJSON a => SResponse -> WaiSession a
    getJsonResponse response = do
      WaiSession $ assertStatus 200 response
      WaiSession $ assertContentType jsonContentType response
      return . fromJust . decode . simpleBody $ response
      where jsonContentType = "application/json; charset=utf-8"

    jsonResponseIs :: (FromJSON a, Eq b, Show a, Show b) => SResponse -> (a -> b) -> b -> WaiSession ()
    jsonResponseIs response f value = do
      decoded <- getJsonResponse response
      liftIO $ decoded `shouldSatisfy` ((==) value . f)

    hasJsonResponse :: (Eq a, Show a, FromJSON a, ToJSON a) => WaiSession SResponse -> a -> WaiSession ()
    hasJsonResponse response x = do
      response' <- getJsonResponse =<< response
      liftIO $ response' `shouldBe` x


suite :: IO TestTree
suite = do
  deckVar <- newIORef testDeck
  testSpec "Game API" (spec deckVar)
