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
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (fromJust)
import System.Random

import Network.Wai.Test (SResponse(..), assertStatus, assertContentType)
import Test.Hspec.Wai hiding (get, post)
import Test.Hspec.Wai.Internal (WaiSession(WaiSession))
import Test.Hspec.Wai.JSON
import Test.Tasty
import Test.Tasty.Hspec

import Haverer.Action (Play(..))
import qualified Haverer.Action as Action
import Haverer.Deck (Card)

import Utils (get, getAs, hazardTestApp, post, postAs, requiresAuth)


-- XXX: All of this guff is so we can actually post a valid move once we've
-- received a response. An alternative would be to pre-program the server to
-- give predictable responses: i.e. rather than shuffling the deck, allow us
-- to specify the order of cards.

data PlayerState = PlayerState (Maybe Card) Int Bool Bool [Card] deriving (Eq, Show)

instance FromJSON PlayerState where

  parseJSON (Object v) = PlayerState <$> v .:? "hand"
                                     <*> v .: "id"
                                     <*> v .: "active"
                                     <*> v .: "protected"
                                     <*> v .: "discards"
  parseJSON _ = mzero


data RoundState = RoundState [PlayerState] Int (Maybe Card) deriving (Eq, Show)

instance FromJSON RoundState where

  parseJSON (Object v) = RoundState <$> v .: "players"
                                    <*> v .: "currentPlayer"
                                    <*> v .:? "dealtCard"


getValidPlays :: RoundState -> [(Card, Action.Play Int)]
getValidPlays (RoundState players i dealtCard) =
  let (Just dealt) = dealtCard
      (PlayerState handCard _ _ _ _) = players !! i
      (Just hand) = handCard
      playerIds = [ pid | (PlayerState _ pid _ _ _) <- players ]
      in Action.getValidPlays i (playerIds \\ [i]) hand dealt


choose :: [a] -> IO a
choose xs = do
  let n = length xs
  i <- getStdRandom (randomR (1, n))
  return (xs !! (i - 1))


playToJSON :: ToJSON a => Maybe (Card, Play a) -> Value
playToJSON move =
  case move of
   Nothing -> object []
   Just (card, NoEffect) -> object ["card" .= card]
   Just (card, Attack target) -> object ["card" .= card, "target" .= target]
   Just (card, Guess target guess) -> object ["card" .= card, "target" .= target,
                                              "guess" .= guess]


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
        [json|{"creator": 0, "state": "pending", "players": [0], "turnTimeout": 3600,
              "numPlayers": 3}|]
        {matchStatus = 201, matchHeaders = ["Location" <:> "/game/0"] }

    it "POST twice creates 2 game" $ do
      post "/users" [json|{username: "foo"}|]
      postAs "foo" "/games" [json|{numPlayers: 3, turnTimeout: 3600}|] `shouldRespondWith`
        [json|{"creator": 0, "state": "pending", "players": [0], "turnTimeout": 3600,
              "numPlayers": 3}|]
        {matchStatus = 201, matchHeaders = ["Location" <:> "/game/0"] }
      postAs "foo" "/games" [json|{numPlayers: 2, turnTimeout: 3600}|] `shouldRespondWith`
        [json|{"creator": 0, "state": "pending", "players": [0], "turnTimeout": 3600,
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
      get (game ++ "/round/0") `hasJsonResponse`
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
                                   currentPlayer: 2}|] { matchStatus = 400 }

    it "POST when you aren't in the game returns error" $ do
      (game, _) <- makeStartedGame 3
      post "/users" [json|{username: "qux"}|]
      postAs "qux" (game ++ "/round/0") [json|{card: "priestess"}|]
        `shouldRespondWith` [json|{message: "You are not playing"}|] { matchStatus = 400 }

    it "POST does *something*" $ do
      (game, [_, _, baz]) <- makeStartedGame 3
      -- XXX: Since we don't know the cards, we don't know how to generate a
      -- valid play. We could maybe try (using Haverer.Testing and the data
      -- from GET), or we could somehow prefab the cards.

      -- XXX: Since we don't know the layout of the cards, we don't know what
      -- the response to the turn is going to be. I guess we should just
      -- assert that it's going to have certain keys.
      let roundUrl = game ++ "/round/0"
          user = encodeUtf8 baz
      (Just response) <- decode <$> simpleBody <$> getAs user roundUrl
      let validPlays = getValidPlays response
      move <- if null validPlays then return Nothing
              else liftIO $ Just <$> choose validPlays
      let message = playToJSON move
      response' <- postAs user roundUrl (encode message)
      jsonResponseIs response' ((["id"] `isInfixOf`) . getKeys) True

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
    makeStartedGame n =
      let userPool = ["foo", "bar", "baz", "qux"]
          users = take n userPool
          creator:others = users
      in do
        game <- makeGameAs creator n
        for_ others (\u -> do post "/users" (encode $ object ["username" .= u])
                              postAs (encodeUtf8 u) game [json|null|] `shouldRespondWith` 200)
        return (game, users)

    getPlayer i v = getKey "players" v >>= \ps -> ps !! i


    getKeys :: Value -> [Text]
    getKeys = fromJust . parseMaybe (withObject "expected object" (return . HashMap.keys))

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
suite = testSpec "Game API" spec
