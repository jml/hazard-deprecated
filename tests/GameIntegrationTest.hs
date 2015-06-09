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
import qualified Data.Text as Text

import Network.Wai.Test (SResponse(..), assertStatus, assertContentType)
import Test.Hspec.Wai hiding (get, post)
import Test.Hspec.Wai.Internal (WaiSession(WaiSession))
import Test.Hspec.Wai.JSON
import Test.Tasty
import Test.Tasty.Hspec

import Haverer.Deck (Card(..), FullDeck)

import qualified Hazard.Routes as Route
import Web.Spock.Safe (renderRoute)

import Utils (get, getAs, hazardTestApp', post, postAs, requiresAuth, makeTestDeck)


testDeck :: FullDeck
testDeck = makeTestDeck "sscmwwskkpcsgspx"



roundUrl i = encodeUtf8 $ renderRoute Route.round 0 i

usersR = encodeUtf8 $ renderRoute Route.users

gamesR = encodeUtf8 $ renderRoute Route.games

gameR i = encodeUtf8 $ renderRoute Route.game i


spec :: IORef FullDeck -> Spec
spec deckVar = with (hazardTestApp' deckVar) $ do
  describe "GET /" $
    it "responds with 200" $
      get "/" `shouldRespondWith` 200

  describe "/games" $ do
    it "GET returns empty list when there are no games" $
      get gamesR `shouldRespondWith` [json|[]|]

    it "POST creates game" $ do
      fooID <- registerUser "foo"
      postAs "foo" gamesR [json|{numPlayers: 3, turnTimeout: 3600}|] `shouldRespondWith`
        (fromValue (
          object [ "creator" .= fooID
                 , "state" .= ("pending" :: Text)
                 , "players" .= object [ fooID .= (Nothing :: Maybe Int) ]
                 , "turnTimeout" .= (3600 :: Int)
                 , "numPlayers" .= (3 :: Int)
                 ]))
        {matchStatus = 201, matchHeaders = ["Location" <:> gameR 0] }

    it "POST twice creates 2 game" $ do
      _ <- registerUser "foo"
      postAs "foo" gamesR [json|{numPlayers: 3, turnTimeout: 3600}|] `shouldRespondWith`
        201 { matchHeaders = ["Location" <:> gameR 0] }
      postAs "foo" gamesR [json|{numPlayers: 3, turnTimeout: 3600}|] `shouldRespondWith`
        201 { matchHeaders = ["Location" <:> gameR 1] }

    it "URLs from POSTs align properly" $ do
      -- Post a 2 player game and 3 player game, and make sure that when we
      -- GET the URLs that number of players is as we requested.
      _ <- registerUser "foo"
      postAs "foo" gamesR [json|{numPlayers: 3, turnTimeout: 3600}|]
      postAs "foo" gamesR [json|{numPlayers: 2, turnTimeout: 3600}|]
      game0 <- get (gameR 0)
      jsonResponseIs game0 (getKey "numPlayers") (Just 3 :: Maybe Int)
      game1 <- get (gameR 1)
      jsonResponseIs game1 (getKey "numPlayers") (Just 2 :: Maybe Int)

    it "unauthenticated POST fails" $
      post gamesR [json|{numPlayers: 3, turnTimeout: 3600}|] `shouldRespondWith` requiresAuth

    it "Created game appears in list" $ do
      _ <- registerUser "foo"
      postAs "foo" gamesR [json|{numPlayers: 3, turnTimeout: 3600}|]
      get gamesR `shouldRespondWith` fromValue (toJSON [renderRoute Route.game 0])

  describe "/game/N" $ do
    it "GET returns 404 if it hasn't been created" $
      get (gameR 0) `shouldRespondWith` 404

    it "POST returns 404 if it hasn't been created" $ do
      _ <- registerUser "foo"
      postAs "foo" (gameR 0) [json|null|] `shouldRespondWith` 404

    it "Created game has POST data" $ do
      fooID <- registerUser "foo"
      game <- makeGameAs' "foo" 3
      get game `shouldRespondWith` fromValue (
        object [
           "numPlayers" .= (3 :: Int),
           "turnTimeout" .= (3600 :: Int),
           "creator" .= fooID,
           "state" .= ("pending" :: Text),
           "players" .= object [
             fooID .= (Nothing :: Maybe Int)
             ]
           ])

    it "POST without authorization fails" $ do
      game <- makeGameAs "foo" 3
      post game [json|null|] `shouldRespondWith` requiresAuth

    it "Can be re-joined by same player" $ do
      fooID <- registerUser "foo"
      game <- makeGameAs' "foo" 3
      postAs "foo" game [json|null|] `shouldRespondWith` fromValue (
        object [
           "numPlayers" .= (3 :: Int),
           "turnTimeout" .= (3600 :: Int),
           "creator" .= fooID,
           "state" .= ("pending" :: Text),
           "players" .= object [
             fooID .= (Nothing :: Maybe Int)
             ]
           ])

    it "POST joins game" $ do
      fooID <- registerUser "foo"
      game <- makeGameAs' "foo" 3
      barID <- registerUser "bar"
      postAs "bar" game [json|null|] `shouldRespondWith` fromValue (
        object [
           "numPlayers" .= (3 :: Int),
           "turnTimeout" .= (3600 :: Int),
           "creator" .= fooID,
           "state" .= ("pending" :: Text),
           "players" .= object [
             fooID .= (Nothing :: Maybe Int),
             barID .= (Nothing :: Maybe Int)
             ]
           ])

    it "POSTing to started game returns bad request" $ do
      game <- makeGameAs "foo" 2
      registerUser "bar"
      postAs "bar" game [json|null|]
      _ <- registerUser "qux"
      postAs "qux" game [json|null|] `shouldRespondWith`
        [json|{message: "Game already started"}|] {matchStatus = 400}

    it "Game starts when enough people have joined" $ do
      fooID <- registerUser "foo"
      game <- makeGameAs' "foo" 2
      barID <- registerUser "bar"
      postAs "bar" game [json|null|] `shouldRespondWith` fromValue (
        object [
           "numPlayers" .= (2 :: Int),
           "turnTimeout" .= (3600 :: Int),
           "creator" .= fooID,
           "state" .= ("in-progress" :: Text),
           "players" .= object [
             fooID .= (0 :: Int),
             barID .= (0 :: Int)
             ],
           "currentRound" .= ("/game/0/round/0" :: Text)
           ])

    it "Same data on GET after POST" $ do
      fooID <- registerUser "foo"
      game <- makeGameAs "foo" 2
      barID <- registerUser "bar"
      postAs "bar" game [json|null|]
      get game `shouldRespondWith` fromValue (
        object [
           "numPlayers" .= (2 :: Int),
           "turnTimeout" .= (3600 :: Int),
           "creator" .= fooID,
           "state" .= ("in-progress" :: Text),
           "players" .= object [
             fooID .= (0 :: Int),
             barID .= (0 :: Int)
             ],
           "currentRound" .= ("/game/0/round/0" :: Text)
           ])

  describe "Playing a game" $ do
    it "Rounds don't exist for unstarted game (GET)" $ do
      game <- makeGameAs "foo" 2
      get (game ++ "/round/0") `shouldRespondWith` 404

    it "Rounds don't exist for unstarted game (POST)" $ do
      game <- makeGameAs "foo" 2
      postAs "foo" (game ++ "/round/0") [json|{card: "priestess"}|] `shouldRespondWith` 404

    it "started game is started" $ do
      (game, [(_, fooID), (_, barID), (_, bazID)]) <- makeStartedGame 3
      get game `shouldRespondWith` (fromValue (
        object [
           "numPlayers" .= (3 :: Int),
           "turnTimeout" .= (3600 :: Int),
           "creator" .= fooID,
           "state" .= ("in-progress" :: Text),
           "players" .= object [
             fooID .= (0 :: Int),
             barID .= (0 :: Int),
             bazID .= (0 :: Int)
             ],
           "currentRound" .= ("/game/0/round/0" :: Text)
           ])) {matchStatus = 200}

    it "Rounds do exist for started games" $ do
      let deck = makeTestDeck "sscmwwskkpcsgspx"
      (game, [(_, fooID), (_, barID), (_, bazID)]) <- makeStartedGame' 3 deck
      get (game ++ "/round/0") `hasJsonResponse`
        object [
          "players" .= [
             object [
                "id" .= fooID,
                "active" .= True,
                "protected" .= False,
                "discards" .= ([] :: [Card])
                ],
             object [
               "id" .= barID,
               "active" .= True,
               "protected" .= False,
               "discards" .= ([] :: [Card])
               ],
             object [
               "id" .= bazID,
               "active" .= True,
               "protected" .= False,
               "discards" .= ([] :: [Card])
               ]
             ],
          -- TODO: Randomize the first player (and the player order).
          -- Currently it's always the *first* person who signed up (i.e. the
          -- creator) followed by other players in signup order.
          "currentPlayer" .= fooID
          ]

    it "Shows your hand when you GET" $ do
      (game, [_, (bar, _), _]) <- makeStartedGame 3
      response <- getAs (encodeUtf8 bar) (game ++ "/round/0")
      jsonResponseIs response (isJust . (getPlayer 1 >=> getCard "hand")) True

    it "Doesn't show other hands when you GET" $ do
      (game, [_, (bar, _), _]) <- makeStartedGame 3
      response <- getAs (encodeUtf8 bar) (game ++ "/round/0")
      jsonResponseIs response (getPlayer 0 >=> getKey "hand") (Nothing :: Maybe Card)
      jsonResponseIs response (getPlayer 2 >=> getKey "hand") (Nothing :: Maybe Card)

    it "Doesn't include dealt card when it's not your turn" $ do
      (game, [_, (bar, _), _]) <- makeStartedGame 3
      response <- getAs (encodeUtf8 bar) (game ++ "/round/0")
      jsonResponseIs response (getKey "dealtCard") (Nothing :: Maybe Card)

    it "Does include dealt card when it is your turn" $ do
      (game, [(foo, _), _, _]) <- makeStartedGame 3
      response <- getAs (encodeUtf8 foo) (game ++ "/round/0")
      jsonResponseIs response (isJust . getCard "dealtCard") True

    it "POST without authorization fails" $ do
      (game, _) <- makeStartedGame 3
      post (game ++ "/round/0") [json|null|] `shouldRespondWith` requiresAuth

    it "POST when it's not your turn returns error" $ do
      (game, [(_, fooID), (bar, _), _]) <- makeStartedGame 3
      postAs (encodeUtf8 bar) (game ++ "/round/0") [json|{card: "priestess"}|]
        `shouldRespondWith`
          (fromValue (object [ "message" .= ("Not your turn" :: Text)
                             , "currentPlayer" .= fooID ])) { matchStatus = 400 }

    it "POST when you aren't in the game returns error" $ do
      (game, _) <- makeStartedGame 3
      _ <- registerUser "qux"
      postAs "qux" (game ++ "/round/0") [json|{card: "priestess"}|]
        `shouldRespondWith` [json|{message: "You are not playing"}|] { matchStatus = 400 }

    it "POST does *something*" $ do
      let deck = makeTestDeck "sscmwwskkpcsgspx"
      (game, [(foo, fooID), _, (_, bazID)]) <- makeStartedGame' 3 deck
      -- Hands are Soldier, Clown, Minister. Player is dealt Wizard.

      -- Play Wizard on baz.
      let roundUrl = game ++ "/round/0"
          user = encodeUtf8 foo
      postAs user roundUrl (fromValue $ object [ "card" .= ("wizard" :: Text)
                                               , "target" .= bazID ])
        `shouldRespondWith` fromValue (object [ "id" .= fooID
                                              , "result" .= ("forced-discard" :: Text)
                                              , "card" .= ("Wizard" :: Text)
                                              , "target" .= bazID ])

    it "ending round reports winner correctly" $ do
      -- XXX: Player foo goes first, and is dealt the first card from the
      -- deck.
      (game, [(foo, fooID), (_, barID)]) <- makeStartedGame' 2 easyToTerminateDeck
      let roundUrl = game ++ "/round/0"
          user = encodeUtf8 foo
      postAs user roundUrl (terminatingPlay barID)
        `shouldRespondWith` (fromValue (
          object [ "id" .= fooID
                 , "result" .= ("eliminated" :: Text)
                 , "card" .= ("Soldier" :: Text)
                 , "guess" .= ("Knight" :: Text)
                 , "target" .= barID
                 , "eliminated" .= barID
                 ])) { matchStatus = 200 }
      -- TODO: Include burn card in serialization of finished round.
      -- TODO: Include survivors in serialization of finished round.

      -- XXX: Maybe the testing strategy here should be to test various
      -- serializations of Round, and use the Haverer testing library to
      -- generate rounds in the states we find interesting?
      getAs user roundUrl `shouldRespondWith` fromValue (
        object [ "currentPlayer" .= (Nothing :: Maybe Text)
               , "winners" .= [fooID]
               , "players" .= [
                    object [ "protected" .= False
                           , "active" .= True
                           , "id" .= fooID
                           , "hand" .= ("Minister" :: Text)
                           , "discards" .= (["Soldier"] :: [Text])
                           ]
                    , object [ "protected" .= (Nothing :: Maybe Bool)
                             , "active" .= False
                             , "id" .= barID
                             , "discards" .= (["Knight"] :: [Text])
                             ]
                    ]
               ])

    it "cannot POST to round after round is over" $ do
      (game, [(foo, _), (_, barID)]) <- makeStartedGame' 2 easyToTerminateDeck
      let roundUrl = game ++ "/round/0"
          user = encodeUtf8 foo
      postAs user roundUrl (terminatingPlay barID)
      postAs user roundUrl (terminatingPlay barID)
        `shouldRespondWith`
        [json|{message: "Round not active"}|] { matchStatus = 400 }

    it "updates game when round is over" $ do
      (game, [(foo, fooID), (_, barID)]) <- makeStartedGame' 2 easyToTerminateDeck
      let user = encodeUtf8 foo
      postAs user (roundUrl 0) (terminatingPlay barID)
      get game `shouldRespondWith` fromValue (
        object [
          "creator" .= fooID,
          "state" .= ("in-progress" :: Text),
          "players" .= object [
            barID .= (0 :: Int),
            fooID .= (1 :: Int)
            ],
          "turnTimeout" .= (3600 :: Int),
          "numPlayers" .= (2 :: Int),
          "currentRound" .= ("/game/0/round/1" :: Text)
          ])

    it "creates new round after previous is over" $ do
      (game, [(foo, fooID), (_, barID)]) <- makeStartedGame' 2 easyToTerminateDeck
      let user = encodeUtf8 foo
      postAs user (roundUrl 0) (terminatingPlay barID)
      let roundUrl2 = game ++ "/round/1"
      get roundUrl2
        `shouldRespondWith` fromValue (
          object [
             "currentPlayer" .= barID,
             "players" .= [
               object [
                 "protected" .= False,
                 "active" .= True,
                 "id" .= fooID,
                 "discards" .= ([] :: [Card])
                 ],
               object [
                 "protected" .= False,
                 "active" .= True,
                 "id" .= barID,
                 "discards" .= ([] :: [Card])
                 ]
               ]
             ])


    it "ends game when final score reached" $ do
      (game, [(foo, fooID), (_, barID)]) <- makeFinishedGame
      get game `shouldRespondWith` fromValue (
        object [
           "creator" .= fooID,
           "state" .= ("finished" :: Text),
           "players" .= object [
             fooID .= (4 :: Int),
             barID .= (3 :: Int)
             ],
           "winners" .= [fooID],
           "turnTimeout" .= (3600 :: Int),
           "numPlayers" .= (2 :: Int)
           ])

    it "serves round information after game is finished" $ do
      (_, [(foo, fooID), (_, barID)]) <- makeFinishedGame
      get (roundUrl 3) `shouldRespondWith` fromValue (
        object [ "currentPlayer" .= (Nothing :: Maybe Text)
               , "winners" .= [barID]
               , "players" .= [
                    object [ "protected" .= (Nothing :: Maybe Bool)
                           , "active" .= False
                           , "id" .= fooID
                           , "discards" .= (["Knight"] :: [Text])
                           ]
                    , object [ "protected" .= False
                             , "active" .= True
                             , "id" .= barID
                             , "discards" .= (["Soldier"] :: [Text])
                             ]
                    ]
               ])

    it "cannot POST to finished games" $ do
      (game, [(foo, _), _]) <- makeFinishedGame
      let user = encodeUtf8 foo
      postAs user game [json|null|] `shouldRespondWith` [json|{message: "Game already finished"}|] { matchStatus = 400 }

  where
    makeGameAs :: Text -> Int -> WaiSession ByteString
    makeGameAs user numPlayers = do
      _ <- registerUser user
      makeGameAs' user numPlayers


    makeGameAs' :: Text -> Int -> WaiSession ByteString
    makeGameAs' user numPlayers = do
      response <- postAs (encodeUtf8 user) gamesR (encode $
                                                     object ["turnTimeout" .= (3600 :: Int)
                                                            ,"numPlayers" .= numPlayers])
      case lookup "Location" (simpleHeaders response) of
       Just path -> return path
       Nothing -> error "Did not create game: could not find return header"


    -- | Register a user and return the ID.
    registerUser :: Text -> WaiSession Text
    registerUser username = do
      response <- post usersR (encode $ object ["username" .= (username :: Text)])
      (return . fromJust) (getKey "id" =<< (decode . simpleBody) response)


    makeStartedGame :: Int -> WaiSession (ByteString, [(Text, Text)])
    makeStartedGame n = makeStartedGame' n testDeck

    makeStartedGame' :: Int -> FullDeck -> WaiSession (ByteString, [(Text, Text)])
    makeStartedGame' n deck =
      let userPool = ["foo", "bar", "baz", "qux"]
          users = take n userPool
          creator:others = users
      in do
        userIDs <- forM users registerUser
        liftIO $ writeIORef deckVar deck
        game <- makeGameAs' creator n
        for_ others (\u -> postAs (encodeUtf8 u) game [json|null|] `shouldRespondWith` 200)
        return (game, zip users userIDs)

    getPlayer i v = getKey "players" v >>= \ps -> ps !! i


    getKey :: FromJSON a => Text -> Value -> Maybe a
    getKey key = parseMaybe (withObject "expected object" (.: key))

    getCard :: Text -> Value -> Maybe Card
    getCard = getKey

    easyToTerminateDeck :: FullDeck
    easyToTerminateDeck = makeTestDeck "cskmwwskspcsgspx"

    terminatingPlay target = encode $ object [ "card" .= ("soldier" :: Text),
                                               "target" .= (target :: Text),
                                               "guess" .= ("knight" :: Text) ]

    makeFinishedGame = do
      (game, users) <- makeStartedGame' 2 easyToTerminateDeck
      let [(foo, fooID), (bar, barID)] = users
          fooUser = encodeUtf8 foo
          barUser = encodeUtf8 bar
      postAs fooUser (roundUrl 0) (terminatingPlay barID)
      postAs barUser (roundUrl 1) (terminatingPlay fooID)
      postAs fooUser (roundUrl 2) (terminatingPlay barID)
      postAs barUser (roundUrl 3) (terminatingPlay fooID)
      postAs fooUser (roundUrl 4) (terminatingPlay barID)
      postAs barUser (roundUrl 5) (terminatingPlay fooID)
      postAs fooUser (roundUrl 6) (terminatingPlay barID)
      postAs barUser (roundUrl 7) (terminatingPlay fooID)
      postAs fooUser (roundUrl 8) (terminatingPlay barID)
      return (game, users)

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
