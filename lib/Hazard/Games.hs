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
                    , Validated(..)
                    , GameError(..)
                    , GameSlot
                    , Game(Pending, InProgress)
                    , JoinError(..)
                    , PlayError(..)
                    , Seconds
                    , SlotAction
                    , SlotAction'
                    , runSlotAction
                    , runSlotAction'
                    , creator
                    , createGame
                    , currentPlayer
                    , gameState
                    , getRound
                    , getPlayers
                    , joinSlot
                    , numPlayers
                    , players
                    , playSlot
                    , requestGame
                    , roundToJSON
                    , turnTimeout
                    , validateCreationRequest
                    , validatePlayRequest
                    ) where

import BasicPrelude hiding (round)

import Control.Error
import Control.Monad.Random
import Control.Monad.State
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), Value(..))
import qualified Data.Map as Map
import qualified Data.Text as Text

import Haverer (
  Card(..),
  Complete,
  Deck,
  Event(..),
  Play(..),
  Player,
  Result(..),
  Round,
  currentPlayer,
  currentTurn,
  getDiscards,
  getPlayers,
  getPlayerMap,
  getHand,
  isProtected,
  newDeck,
  toPlayers,
  toPlayerSet,
  viewAction
  )
import qualified Haverer.Game as H
import qualified Haverer.Player as P
import qualified Haverer.Round as Round


data GameError a = GameNotFound Int
                 | JoinError (JoinError a)
                 | PlayError (PlayError a)
                 deriving (Show)

data JoinError a = AlreadyStarted
                 | InvalidPlayers (P.Error a)
                 deriving (Eq, Show)

data PlayError a = NotStarted
                 | PlayNotSpecified
                 | BadAction (Round.BadAction a)
                 | NotYourTurn a a
                 | NotInGame a
                 | RoundNotFound Int
                 | RoundNotActive
                 deriving (Show)


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
            | Ready { _playerSet :: P.PlayerSet a }
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
        players' (Ready { _playerSet = _playerSet }) = toPlayers _playerSet
        players' (InProgress { game = game }) = (toPlayers . H.players) game


getRound :: Game a -> Int -> Maybe (Round a)
getRound InProgress { rounds = rounds } i = atMay rounds i
getRound _ _ = Nothing


type SlotAction g p a = StateT (GameSlot p) (RandT g (Either (GameError p))) a

type SlotAction' p a = StateT (GameSlot p) (Either (GameError p)) a


runSlotAction :: RandomGen g => SlotAction g p a -> g -> GameSlot p -> Either (GameError p) (a, GameSlot p)
runSlotAction action gen slot = evalRandT (runStateT action slot) gen


runSlotAction' :: SlotAction' p a -> GameSlot p -> Either (GameError p) (a, GameSlot p)
runSlotAction' = runStateT


modifyGame :: (Game a -> RandT g (Either (GameError a)) (Game a)) -> SlotAction g a ()
modifyGame f = do
  slot <- get
  let game = gameState slot
  game'' <- (lift . f) game
  put (slot { gameState = game'' })


joinSlot :: (RandomGen g, Ord p, Show p) => p -> SlotAction g p ()
joinSlot p = modifyGame $ \game ->
  do
    game' <- (lift . fmapL JoinError . joinGame p) game
    case game' of
     Ready playerSet -> do
       deck <- newDeck
       return $ makeGame deck playerSet
     _ -> return game'


joinGame :: (Show a, Ord a) => a -> Game a -> Either (JoinError a) (Game a)
joinGame _ (InProgress {}) = Left AlreadyStarted
joinGame _ (Ready {}) = Left AlreadyStarted
joinGame p g@(Pending {..})
  | p `elem` _players = return g
  | numNewPlayers == _numPlayers =
      Ready <$> fmapL InvalidPlayers (toPlayerSet newPlayers)
  | otherwise = return Pending { _numPlayers = _numPlayers
                               , _players = newPlayers }
  where newPlayers = p:_players
        numNewPlayers = length newPlayers


makeGame :: (Show a, Ord a) => Deck Complete -> P.PlayerSet a -> Game a
makeGame deck playerSet = do
  let game = H.makeGame playerSet
  let round = H.newRound' game deck
  InProgress { game = game, rounds = pure round }


startRound :: (Ord a, Show a) => Deck Complete -> Game a -> Either (PlayError a) (Game a)
startRound _ (Pending {}) = Left NotStarted
startRound _ (Ready {}) = Left NotStarted
startRound deck g@(InProgress { .. }) =
  return g { rounds = rounds ++ pure (H.newRound' game deck) }


validatePlayRequest :: Eq a => a -> Int -> Maybe (PlayRequest a) -> SlotAction' a (Maybe (PlayRequest a))
validatePlayRequest player roundId request = do
  game <- gameState <$> get
  round <- lift $ note (PlayError (RoundNotFound roundId)) (getRound game roundId)
  unless (player `elem` getPlayers round) (throwError (NotInGame player))
  current <- lift $ note (PlayError RoundNotActive) (currentPlayer round)
  unless (player == current) (throwError (NotYourTurn player current))
  return request

  where throwError = lift . fmapL PlayError . Left


playSlot :: (Ord a, Show a) => Maybe (PlayRequest a) -> SlotAction' a (Result a)
playSlot playRequest = do
  slot <- get
  case gameState slot of
   Pending {} -> throwError NotStarted
   Ready {} -> throwError NotStarted
   InProgress {..} -> do
     let round = head rounds
     (result, round') <- (lift . fmapL (PlayError . BadAction) . Round.playTurn' round . fmap requestToPlay) playRequest
     put (slot { gameState = InProgress { game = game, rounds = round':tail rounds } })
     return result
  where throwError = lift . fmapL PlayError . Left
        requestToPlay (PlayRequest card p) = (card, p)
