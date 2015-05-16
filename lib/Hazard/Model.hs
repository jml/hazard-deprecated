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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Hazard.Model ( GameCreationError(..)
                    , GameCreationRequest(reqNumPlayers, reqTurnTimeout)
                    , GameSlot
                    , Game(Pending, InProgress)
                    , JoinError(..)
                    , Seconds
                    , Validated(..)
                    , creator
                    , createGame
                    , gameState
                    , getRound
                    , joinGame
                    , numPlayers
                    , players
                    , requestGame
                    , turnTimeout
                    , validateCreationRequest
                    ) where

import Prelude hiding (round)

import Control.Monad.Random (MonadRandom)
import Data.Aeson (ToJSON(..), object, (.=), Value)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Haverer.Game as H


import Haverer.Deck (Card)
import Haverer.Player (Player, getDiscards, getHand, isProtected, toPlayers, toPlayerSet)
import Haverer.Round (currentPlayer, getPlayerMap, Round)


type Seconds = Int


data Validated = Unchecked | Valid


data GameCreationError = InvalidNumberOfPlayers Int
                       | InvalidTurnTimeout Seconds
                       deriving (Eq, Show)


data GameCreationRequest (a :: Validated) = GameCreationRequest {
  reqNumPlayers :: Int,
  reqTurnTimeout :: Seconds
  } deriving (Eq, Show)


requestGame :: Int -> Seconds -> GameCreationRequest 'Unchecked
requestGame = GameCreationRequest


validateCreationRequest :: GameCreationRequest 'Unchecked -> Either GameCreationError (GameCreationRequest 'Valid)
validateCreationRequest (GameCreationRequest { .. })
  | reqTurnTimeout <= 0 = Left $ InvalidTurnTimeout reqTurnTimeout
  | reqNumPlayers < 2 = Left $ InvalidNumberOfPlayers reqNumPlayers
  | reqNumPlayers > 4 = Left $ InvalidNumberOfPlayers reqNumPlayers
  | otherwise = Right $ GameCreationRequest reqNumPlayers reqTurnTimeout


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
    case gameState slot of
     Pending {} -> object [ "turnTimeout" .= turnTimeout slot
                          , "creator" .= creator slot
                          , "state" .= ("pending" :: Text)
                          , "numPlayers" .= numPlayers slot
                          , "players" .= players slot
                          ]
     InProgress {} -> object [ "turnTimeout" .= turnTimeout slot
                             , "creator" .= creator slot
                             , "state" .= ("in-progress" :: Text)
                             , "numPlayers" .= numPlayers slot
                             , "players" .= players slot
                             , "scores" .= replicate (length (players slot)) (0 :: Int)
                             ]


instance ToJSON a => ToJSON (Round a) where
  toJSON round = object [ "players" .= (map (uncurry playerToJSON) .  Map.assocs . getPlayerMap) round
                        , "currentPlayer" .= currentPlayer round
                        ]


playerToJSON :: ToJSON a => a -> Player -> Value
playerToJSON pid player =
  object [ "id" .= pid
         , "active" .= (isJust . getHand) player
         , "protected" .= isProtected player
         , "discards" .= getDiscards player
         ]


instance ToJSON Card where

  toJSON = toJSON . show


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
getRound InProgress { rounds = rounds } i
  | 0 <= i && i < length rounds = Just $ rounds !! i
  | otherwise = Nothing
getRound _ _ = Nothing


data JoinError = AlreadyStarted | AlreadyJoined deriving (Eq, Show)

joinGame :: (MonadRandom m, Show a, Ord a) => GameSlot a -> a -> Either JoinError (m (GameSlot a))
joinGame slot p = do
  newState <- joinGame' (gameState slot) p
  return $ do
    newState' <- newState
    return $ slot { gameState = newState' }


joinGame' :: (Show a, Ord a, MonadRandom m) => Game a -> a -> Either JoinError (m (Game a))
joinGame' (InProgress {}) _ = Left AlreadyStarted
joinGame' (Pending {..}) p
  | p `elem` _players = Left AlreadyJoined
  | numNewPlayers == _numPlayers = Right $ do
      round <- H.newRound newGame
      return $ InProgress newGame (pure round)
  | otherwise = Right $ return Pending { _numPlayers = _numPlayers
                                       , _players = newPlayers }
  where newPlayers = p:_players
        numNewPlayers = length newPlayers
        newGame = case toPlayerSet newPlayers of
                   Left e -> error $ "Couldn't make game: " ++ show e
                   Right r -> H.makeGame r
