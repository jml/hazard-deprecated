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
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.Except
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
                 | OtherError a
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


validateCreationRequest :: MonadError GameCreationError m => GameCreationRequest 'Unchecked -> m (GameCreationRequest 'Valid)
validateCreationRequest (GameCreationRequest { .. })
  | reqTurnTimeout <= 0 = throwError $ InvalidTurnTimeout reqTurnTimeout
  | reqNumPlayers < 2 = throwError $ InvalidNumberOfPlayers reqNumPlayers
  | reqNumPlayers > 4 = throwError $ InvalidNumberOfPlayers reqNumPlayers
  | otherwise = return $ GameCreationRequest reqNumPlayers reqTurnTimeout


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


type SlotAction e g p a = StateT (GameSlot p) (RandT g (Either (GameError e))) a

type SlotAction' e p a = StateT (GameSlot p) (Either (GameError e)) a


runSlotAction :: RandomGen g => SlotAction e g p a -> g -> GameSlot p -> Either (GameError e) (a, GameSlot p)
runSlotAction action gen slot = evalRandT (runStateT action slot) gen


runSlotAction' :: SlotAction' e p a -> GameSlot p -> Either (GameError e) (a, GameSlot p)
runSlotAction' = runStateT


liftEither :: MonadTrans t => Either e a -> t (Either (GameError e)) a
liftEither = lift . fmapL OtherError


liftMaybe :: MonadTrans t => e -> Maybe a -> t (Either (GameError e)) a
liftMaybe e = liftEither . note e


throwOtherError :: MonadError (GameError e) m => e -> m a
throwOtherError = throwError . OtherError


modifyGame :: (Game a -> RandT g (Either (GameError e)) (Game a)) -> SlotAction e g a ()
modifyGame f = do
  slot <- get
  let game = gameState slot
  game'' <- (lift . f) game
  put (slot { gameState = game'' })


joinSlot :: (RandomGen g, Ord p, Show p) => p -> SlotAction (JoinError p) g p ()
joinSlot p = modifyGame $ \game ->
  do
    game' <- (liftEither . joinGame p) game
    case game' of
     Ready playerSet -> do
       deck <- newDeck
       return $ makeGame deck playerSet
     _ -> return game'


joinGame :: (Show a, Ord a) => a -> Game a -> Either (JoinError a) (Game a)
joinGame _ (InProgress {}) = throwError AlreadyStarted
joinGame _ (Ready {}) = throwError AlreadyStarted
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


validatePlayRequest :: Eq a => a -> Int -> Maybe (PlayRequest a) -> SlotAction' (PlayError a) a (Maybe (PlayRequest a))
validatePlayRequest player roundId request = do
  game <- gameState <$> get
  round <- liftMaybe (RoundNotFound roundId) (getRound game roundId)
  unless (player `elem` getPlayers round) (throwOtherError (NotInGame player))
  current <- liftMaybe RoundNotActive (currentPlayer round)
  unless (player == current) (throwOtherError (NotYourTurn player current))
  return request


playSlot :: (Ord a, Show a) => Maybe (PlayRequest a) -> SlotAction' (PlayError a) a (Result a)
playSlot playRequest = do
  slot <- get
  case gameState slot of
   Pending {} -> throwOtherError NotStarted
   Ready {} -> throwOtherError NotStarted
   InProgress {..} -> do
     let round = head rounds
     (result, round') <- playTurnOn round playRequest
     put (slot { gameState = InProgress { game = game, rounds = round':tail rounds } })
     return result
  where requestToPlay (PlayRequest card p) = (card, p)
        playTurnOn round = liftEither . fmapL BadAction . Round.playTurn' round . fmap requestToPlay
