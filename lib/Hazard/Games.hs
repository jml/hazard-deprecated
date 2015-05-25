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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hazard.Games ( GameCreationError(..)
                    , GameCreationRequest(reqNumPlayers, reqTurnTimeout)
                    , GameSlot
                    , Game(Pending, InProgress)
                    , JoinError(..)
                    , Seconds
                    , Validated(..)
                    , creator
                    , createGame
                    , currentPlayer
                    , gameState
                    , getRound
                    , getPlayers
                    , joinGame
                    , numPlayers
                    , players
                    , playTurn
                    , requestGame
                    , roundToJSON
                    , turnTimeout
                    , validateCreationRequest
                    ) where

import BasicPrelude hiding (round)

import Control.Error
import Control.Monad.Random (MonadRandom)
import Control.Monad.State
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), Value(..))
import qualified Data.Map as Map
import qualified Data.Text as Text

import Haverer.Action (Play(..), viewAction)
import qualified Haverer.Game as H
import Haverer.Internal.Error
import Haverer.Deck (Card(..))
import Haverer.Player (Player, getDiscards, getHand, isProtected, toPlayers, toPlayerSet)
import qualified Haverer.Round as Round
import Haverer.Round (currentPlayer, currentTurn, getPlayers, getPlayerMap, Round, Event(..))


type Seconds = Int


data Validated = Unchecked | Valid


data GameCreationError = InvalidNumberOfPlayers Int
                       | InvalidTurnTimeout Seconds
                       deriving (Eq, Show)


data GameCreationRequest (a :: Validated) = GameCreationRequest {
  reqNumPlayers :: Int,
  reqTurnTimeout :: Seconds
  } deriving (Eq, Show)


instance ToJSON (GameCreationRequest a) where
  toJSON r = object [ "numPlayers" .= reqNumPlayers r
                    , "turnTimeout" .= reqTurnTimeout r
                    ]

instance FromJSON (GameCreationRequest 'Unchecked) where
  parseJSON (Object v) = requestGame <$> v .: "numPlayers" <*> v .: "turnTimeout"
  parseJSON _ = mzero


requestGame :: Int -> Seconds -> GameCreationRequest 'Unchecked
requestGame = GameCreationRequest


validateCreationRequest :: GameCreationRequest 'Unchecked -> Either GameCreationError (GameCreationRequest 'Valid)
validateCreationRequest (GameCreationRequest { .. })
  | reqTurnTimeout <= 0 = Left $ InvalidTurnTimeout reqTurnTimeout
  | reqNumPlayers < 2 = Left $ InvalidNumberOfPlayers reqNumPlayers
  | reqNumPlayers > 4 = Left $ InvalidNumberOfPlayers reqNumPlayers
  | otherwise = Right $ GameCreationRequest reqNumPlayers reqTurnTimeout


data PlayRequest a = PlayRequest Card (Play a) deriving (Eq, Show)


instance FromJSON a => FromJSON (PlayRequest a) where
  parseJSON (Object v) = do
    -- XXX: Find out how to do this all in one line into a tuple using
    -- applicative functor
    card <- v .: "card"
    target <- v .:? "target"
    guess <- v .:? "guess"
    PlayRequest card <$> case (target, guess) of
     (Nothing, Nothing) -> return NoEffect
     (Just player, Nothing) -> return $ Attack player
     (Just player, Just guess') -> return $ Guess player guess'
     _ -> mzero
  parseJSON _ = mzero


-- XXX: I'm ambivalent about having GameSlot & Game. The idea is that the
-- fields in GameSlot are all metadata, and entirely irrelevant to the "rules"
-- bit in gameState.

data GameSlot a = GameSlot {
  turnTimeout :: Seconds,
  creator :: a,
  gameState :: Game a
  } deriving (Show)


data Game a = Pending { _numPlayers :: Int
                      , _players :: [a]
                      }
            | InProgress { game :: H.Game a
                         , rounds :: [Round a]
                         }
            deriving (Show)


instance ToJSON a => ToJSON (GameSlot a) where
  toJSON slot =
    object (specificFields ++ commonFields)
    where
      specificFields =
        case gameState slot of
         Pending {} -> ["state" .= ("pending" :: Text)]
         InProgress {} -> [ "state" .= ("in-progress" :: Text)
                          , "scores" .= replicate (length (players slot)) (0 :: Int)
                          ]
      commonFields = [ "turnTimeout" .= turnTimeout slot
                     , "creator" .= creator slot
                     , "numPlayers" .= numPlayers slot
                     , "players" .= players slot
                     ]


instance (Ord a, ToJSON a) => ToJSON (Round a) where
  toJSON = roundToJSON Nothing


roundToJSON :: (Ord a, ToJSON a) => Maybe a -> Round a -> Value
roundToJSON someone round =
  object $ [ "players" .= (map playerToJSON' .  Map.assocs . getPlayerMap) round
           , "currentPlayer" .= currentPlayer round
           ] ++ (("dealtCard" .=) <$> justZ (getDealt someone round))
  where playerToJSON' = uncurry (playerToJSON someone)


getDealt :: (MonadPlus m, Ord a) => m a -> Round a -> m Card
getDealt someone round = do
  (pid, (dealt, _)) <- justZ (currentTurn round)
  viewer <- someone
  guard (viewer == pid)
  return dealt


playerToJSON :: (Eq a, ToJSON a) => Maybe a -> a -> Player -> Value
playerToJSON someone pid player =
  case someone of
   Nothing -> object commonFields
   Just viewer
     | viewer == pid -> object $ ("hand" .= getHand player):commonFields
     | otherwise -> playerToJSON Nothing pid player
  where commonFields =
          [ "id" .= pid
          , "active" .= (isJust . getHand) player
          , "protected" .= isProtected player
          , "discards" .= getDiscards player
          ]


instance ToJSON Card where

  toJSON = toJSON . show


instance ToJSON a => ToJSON (Round.Result a) where

  toJSON (Round.BustedOut playerId dealt hand) =
    object [ "result" .= ("busted" :: Text)
           , "id" .= playerId
           , "dealt" .= dealt
           , "hand" .= hand
           ]
  toJSON (Round.Played action event) =
    object $ [ "result" .= ("played" :: Text) ] ++ actions ++ events
    where
      actions =
        let (pid, card, play) = viewAction action in
         [ "id" .= pid, "card" .= card ] ++
         case play of
          NoEffect -> []
          Attack target -> ["target" .= target]
          Guess target guess -> ["target" .= target, "guess" .= guess]
      events = case event of
                NoChange -> [ "result" .= ("no-change" :: Text) ]
                Protected pid -> [ "result" .= ("protected" :: Text)
                                 , "protected" .= pid
                                 ]
                SwappedHands tgt src -> [ "result" .= ("swapped-hands" :: Text)
                                        , "swapped-hands" .= [src, tgt]
                                        ]
                Eliminated pid -> [ "result" .= ("eliminated" :: Text)
                                  , "eliminated" .= pid ]
                ForcedDiscard {} -> [ "result" .= ("forced-discard" :: Text) ]
                ForcedReveal src tgt _ -> [ "result" .= ("forced-reveal" :: Text)
                                          , "forced-reveal" .= [src, tgt]
                                          ]

instance FromJSON Card where

  parseJSON (String s) =
    case Text.toLower s of
     "soldier"   -> return Soldier
     "clown"     -> return Clown
     "knight"    -> return Knight
     "priestess" -> return Priestess
     "wizard"    -> return Wizard
     "general"   -> return General
     "minister"  -> return Minister
     "prince"    -> return Prince
     _ -> mzero
  parseJSON _ = mzero


createGame :: a -> GameCreationRequest 'Valid -> GameSlot a
createGame userId request = GameSlot { turnTimeout = reqTurnTimeout request
                                     , creator = userId
                                     , gameState = Pending { _numPlayers = reqNumPlayers request
                                                           , _players = [userId]
                                                           }
                                     }


numPlayers :: GameSlot a -> Int
numPlayers =
  numPlayers' . gameState
  where numPlayers' (Pending { _numPlayers = _numPlayers }) = _numPlayers
        numPlayers' (InProgress { game = game' }) = (length . toPlayers . H.players) game'


players :: GameSlot a -> [a]
players =
  players' . gameState
  where players' (Pending { _players = _players }) = _players
        players' (InProgress { game = game }) = (toPlayers . H.players) game


getRound :: Game a -> Int -> Maybe (Round a)
getRound InProgress { rounds = rounds } i = atMay rounds i
getRound _ _ = Nothing


data JoinError a = AlreadyStarted | AlreadyJoined a deriving (Eq, Show)

joinGame :: (MonadRandom m, Show a, Ord a) => GameSlot a -> a -> Either (JoinError a) (m (GameSlot a))
joinGame slot p = do
  newState <- joinGame' (gameState slot) p
  return $ do
    newState' <- newState
    return $ slot { gameState = newState' }


joinGame' :: (Show a, Ord a, MonadRandom m) => Game a -> a -> Either (JoinError a) (m (Game a))
joinGame' (InProgress {}) _ = Left AlreadyStarted
joinGame' (Pending {..}) p
  | p `elem` _players = Left (AlreadyJoined p)
  | numNewPlayers == _numPlayers = return $ InProgress newGame <$> pure <$> H.newRound newGame
  | otherwise = Right $ return Pending { _numPlayers = _numPlayers
                                       , _players = newPlayers }
  where newPlayers = p:_players
        numNewPlayers = length newPlayers
        newGame = assertRight "Couldn't make game: " (H.makeGame <$> toPlayerSet newPlayers)


data PlayError a = NotStarted | PlayNotSpecified | BadAction (Round.BadAction a) deriving Show


playTurn :: (Ord a, Show a) => GameSlot a -> Maybe (PlayRequest a) -> Either (PlayError a) (Round.Result a, GameSlot a)
playTurn slot playRequest = flip runStateT slot $
  case gameState slot of
   Pending {} -> lift $ Left NotStarted
   InProgress {..} ->
     let round = head rounds in
     do
       (result, round') <- lift $ fmapL BadAction $ Round.playTurn' round (requestToPlay <$> playRequest)
       put (slot { gameState = InProgress { game = game, rounds = round':tail rounds } })
       return result


requestToPlay :: PlayRequest a -> (Card, Play a)
requestToPlay (PlayRequest card p) = (card, p)
