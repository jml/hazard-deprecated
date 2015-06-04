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
                    , GameID
                    , GameSlot
                    , Game(Pending, InProgress)
                    , JoinError(..)
                    , PlayError(..)
                    , RoundID
                    , Seconds
                    , SlotAction
                    , runSlotAction
                    , runSlotActionT
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
import Control.Monad.Identity
import Control.Monad.State
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), Value(..))
import qualified Data.Map as Map
import qualified Data.Text as Text
import Web.Spock.Safe (renderRoute)

import qualified Hazard.Routes as Route
import Hazard.Users (UserID, toJSONKey)

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

data JoinError = AlreadyStarted
               | InvalidPlayers (P.Error UserID)
               | AlreadyFinished
               deriving (Eq, Show)

data PlayError = NotStarted
               | PlayNotSpecified
               | RoundFinished
               | BadAction (Round.BadAction UserID)
               | NotYourTurn UserID UserID
               | NotInGame UserID
               | RoundNotFound Int
               | RoundNotActive
               deriving (Show)


type GameID = Int
type RoundID = Int

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


data PlayRequest = PlayRequest Card (Play UserID) deriving (Eq, Show)


instance FromJSON PlayRequest where
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


-- | Represents a single game.
--
-- Pretty much all of the interesting state about the game is in 'Game'.
data GameSlot = GameSlot {
  gameID :: GameID,
  turnTimeout :: Seconds,
  creator :: UserID,
  gameState :: Game
  } deriving (Show)


data Game = Pending { _numPlayers :: Int
                    , _players :: [UserID]
                    }
          | Ready { _playerSet :: P.PlayerSet UserID }
          | InProgress { game :: H.Game UserID
                       , rounds :: [Round UserID]
                       }
          | Finished { _outcome :: H.Outcome UserID
                     , rounds :: [Round UserID]
                     }
          deriving (Show)


instance ToJSON GameSlot where
  toJSON slot =
    object (specificFields ++ commonFields)
    where
      specificFields =
        case gameState slot of
         Pending {} -> ["state" .= ("pending" :: Text)]
         Ready {} -> ["state" .= ("pending" :: Text)]
         InProgress {..} -> [ "state" .= ("in-progress" :: Text)
                            , "currentRound" .= renderRoute Route.round (gameID slot) (length rounds - 1)
                            ]
         Finished {..} -> [ "state" .= ("finished" :: Text)
                          , "winners" .= H.winners _outcome
                          ]
      commonFields = [ "turnTimeout" .= turnTimeout slot
                     , "creator" .= creator slot
                     , "numPlayers" .= numPlayers slot
                     , "players" .= Map.mapKeys toJSONKey (getScores (gameState slot))
                     ]


instance (Ord a, ToJSON a) => ToJSON (Round a) where
  toJSON = roundToJSON Nothing


roundToJSON :: (Ord a, ToJSON a) => Maybe a -> Round a -> Value
roundToJSON someone round =
  object $ [ "players" .= (map playerToJSON' .  Map.assocs . getPlayerMap) round
           , "currentPlayer" .= currentPlayer round
           ] ++ msum [("dealtCard" .=) <$> justZ getDealt
                     ,("winners" .=) <$> justZ getWinners]
  where
    playerToJSON' = uncurry (playerToJSON someone)

    getDealt = do
      (pid, (dealt, _)) <- justZ (currentTurn round)
      viewer <- someone
      guard (viewer == pid)
      return dealt

    getWinners = Round.getWinners <$> Round.victory round


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


createGame :: UserID -> GameID -> GameCreationRequest 'Valid -> GameSlot
createGame userId gameId request =
  GameSlot { gameID = gameId
           , turnTimeout = reqTurnTimeout request
           , creator = userId
           , gameState = Pending { _numPlayers = reqNumPlayers request
                                 , _players = [userId]
                                 }
           }


numPlayers :: GameSlot -> Int
numPlayers =
  numPlayers' . gameState
  where numPlayers' (Pending { _numPlayers = _numPlayers }) = _numPlayers
        numPlayers' (Ready { _playerSet = _playerSet }) = (length . toPlayers) _playerSet
        numPlayers' (InProgress { game = game' }) = (length . toPlayers . H.players) game'
        numPlayers' (Finished { _outcome = outcome }) = (length . H.finalScores) outcome


players :: GameSlot -> [UserID]
players = players' . gameState

players' :: Game -> [UserID]
players' (Pending { _players = _players }) = _players
players' (Ready { _playerSet = _playerSet }) = toPlayers _playerSet
players' (InProgress { game = game }) = (toPlayers . H.players) game
players' (Finished { _outcome = outcome }) = map fst . H.finalScores $ outcome


getRound :: Game -> Int -> Maybe (Round UserID)
getRound InProgress { rounds = rounds } i = atMay rounds i
getRound Finished { rounds = rounds } i = atMay rounds i
getRound _ _ = Nothing


getScores :: Game -> Map UserID (Maybe Int)
getScores game =
  Map.fromList $ case game of
   Pending {} -> zip (players' game) (repeat Nothing)
   Ready {} -> zip (players' game) (repeat (Just 0))
   InProgress { game = game' } -> map (second Just) (H.scores game')
   Finished { _outcome = outcome } -> map (second Just) (H.finalScores outcome)


type SlotActionT e m a = StateT GameSlot (EitherT (GameError e) m) a
type SlotAction e a = SlotActionT e Identity a


runSlotActionT :: SlotActionT e m a -> GameSlot -> m (Either (GameError e) (a, GameSlot))
runSlotActionT action slot = runEitherT (runStateT action slot)


runSlotAction :: SlotAction e a -> GameSlot -> Either (GameError e) (a, GameSlot)
runSlotAction action slot = runIdentity $ runSlotActionT action slot


liftEither :: (Monad m, MonadTrans t) => EitherT e m a -> t (EitherT (GameError e) m) a
liftEither = lift . fmapLT OtherError


throwOtherError :: MonadError (GameError e) m => e -> m a
throwOtherError = throwError . OtherError


modifyGame :: Monad m => (Game -> EitherT (GameError e) m Game) -> SlotActionT e m ()
modifyGame f = do
  slot <- get
  let game = gameState slot
  game'' <- (lift . f) game
  put (slot { gameState = game'' })


joinSlot :: Deck Complete -> UserID -> SlotAction JoinError ()
joinSlot deck p = modifyGame $ \game ->
  do
    game' <- (fmapLT OtherError . hoistEither . joinGame p) game
    case game' of
     Ready playerSet -> return $ makeGame deck playerSet
     _ -> return game'


joinGame :: UserID -> Game -> Either JoinError Game
joinGame _ (InProgress {}) = throwError AlreadyStarted
joinGame _ (Ready {}) = throwError AlreadyStarted
joinGame _ (Finished {}) = throwError AlreadyFinished
joinGame p g@(Pending {..})
  | p `elem` _players = return g
  | numNewPlayers == _numPlayers =
      Ready <$> fmapL InvalidPlayers (toPlayerSet newPlayers)
  | otherwise = return Pending { _numPlayers = _numPlayers
                               , _players = newPlayers }
  where newPlayers = _players ++ [p]
        numNewPlayers = length newPlayers


makeGame :: Deck Complete -> P.PlayerSet UserID -> Game
makeGame deck playerSet = do
  let game = H.makeGame playerSet
  let round = H.newRound' game deck
  InProgress { game = game, rounds = pure round }


validatePlayRequest :: UserID -> Int -> Maybe PlayRequest -> SlotAction PlayError (Maybe PlayRequest)
validatePlayRequest player roundId request = do
  game <- gameState <$> get
  round <- liftEither $ getRound game roundId ?? RoundNotFound roundId
  unless (player `elem` getPlayers round) (throwOtherError (NotInGame player))
  current <- liftEither $ currentPlayer round ?? RoundNotActive
  unless (player == current) (throwOtherError (NotYourTurn player current))
  return request


playSlot :: Deck Complete -> Maybe PlayRequest -> SlotAction PlayError (Result UserID)
playSlot deck playRequest = do
  currentState <- gameState <$> get
  case currentState of
   Pending {} -> throwOtherError NotStarted
   Ready {} -> throwOtherError NotStarted
   Finished {} -> throwOtherError RoundFinished
   InProgress {} -> do
     let round = last . rounds $ currentState
     (result, round') <- playTurnOn round playRequest
     modify $ \s -> s { gameState = (gameState s) { rounds = init (rounds . gameState $ s) ++ [round'] } }
     case Round.victory round' of
      Nothing -> return ()
      Just victory -> do
        game' <- game . gameState <$> get
        case H.playersWon game' (Round.getWinners victory) of
         Left outcome ->
           modify $ \s -> s { gameState = Finished outcome (rounds (gameState s)) }
         Right game'' -> do
           modify $ \s -> s { gameState = (gameState s) { game = game'' } }
           modify $ \s -> s { gameState = addRound (H.newRound' (game . gameState $ s) deck) (gameState s) }
     return result
  where
    requestToPlay (PlayRequest card p) = (card, p)
    playTurnOn round = liftEither . fmapLT BadAction . hoistEither . Round.playTurn' round . fmap requestToPlay
    addRound round toState = toState { rounds = rounds toState ++ [round] }
