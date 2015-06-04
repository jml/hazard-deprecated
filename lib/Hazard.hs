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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hazard ( makeHazard
              , hazardWeb
              , hazardWeb'
              ) where


import BasicPrelude hiding (round)

import Control.Error
import Control.Monad.Random (evalRandIO)
import Control.Monad.STM (atomically)

import Data.Aeson (FromJSON(..), (.=), object)
import Network.HTTP.Types.Status
import Network.Wai (requestMethod, pathInfo)
import Network.Wai.Middleware.HttpAuth

import Web.Spock.Safe

import Haverer (Complete, Deck, newDeck)
import Hazard.HttpAuth (maybeLoggedIn)

import Hazard.Model (
  Hazard,
  createGame,
  getGameSlot,
  getGames,
  getRound,
  makeHazard,
  applySlotAction,
  runSlotAction,
  tryGetSlot,
  users
  )
import qualified Hazard.Views as View

import Hazard.Games (
  JoinError(..),
  GameError(..),
  PlayError(..),
  joinSlot,
  playSlot,
  roundToJSON,
  validateCreationRequest,
  validatePlayRequest
  )
import qualified Hazard.Routes as Route
import Hazard.Users (
  UserDB,
  UserID,
  addUser,
  authenticate,
  getUserByID,
  getUserIDByName,
  makePassword,
  getAllUsers
  )


expectJSON :: (MonadIO m, FromJSON a) => ActionT m a
expectJSON = do
  -- XXX: jsonBody appears to be using a parser that's really fussy about its
  -- JSON: no whitespace, must quote object keys.
  body' <- jsonBody
  case body' of
   Nothing -> do
     setStatus badRequest400
     text "Expected JSON, but could not parse it: try removing whitespace?"
   Just contents -> return contents


userWeb :: MonadIO m => UserDB -> IO ByteString -> SpockT m ()
userWeb userDB pwgen = do

  middleware $ basicAuth (authUserDB userDB) ("" { authRealm = encodeUtf8 View.realm,
                                                   authIsProtected = isProtected })

  get Route.users $ do
    users' <- liftIO $ atomically $ getAllUsers userDB
    View.users $ map (second decodeUtf8) users'

  post Route.users $ do
    userRequest <- expectJSON
    password <- liftIO pwgen
    newID <- liftIO $ atomically $ addUser userDB userRequest password
    case newID of
     Just newID' -> do
       setStatus created201
       setHeader "Location" (renderRoute Route.user newID')
       json (object ["password" .= decodeUtf8 password
                    ,"id" .= newID'])
     Nothing -> View.badRequest ("username already exists" :: Text)

  get Route.user $ \userID -> do
    user <- liftIO $ atomically $ getUserByID userDB userID
    case user of
     Just user' -> json user'
     Nothing -> View.errorMessage notFound404 ("no such user" :: Text)

  where
    isProtected req =
      return $ case (requestMethod req, pathInfo req) of
                -- Must be able to register without first being authenticated.
                ("POST", "users":_) -> False
                -- All other POSTs (i.e. writes) require authentication.
                ("POST", _) -> True
                -- Can't get user details without first being registered.
                (_, "user":_) -> True
                -- Everything else is publicly available.
                _ -> False


authUserDB :: UserDB -> CheckCreds
authUserDB userDB username password = do
  found <- liftIO $ atomically $ authenticate userDB username password
  return (isJust found)


maybeLoggedInUser :: MonadIO m => UserDB -> ActionT m (Maybe UserID)
maybeLoggedInUser userDB = do
  req <- request
  case maybeLoggedIn req of
   Just user -> liftIO $ atomically $ getUserIDByName userDB user
   Nothing -> return Nothing


withAuth :: MonadIO m => UserDB -> (UserID -> ActionT m ()) -> ActionT m ()
withAuth userDB action = maybeLoggedInUser userDB >>= maybe View.authenticationRequired action


hazardWeb :: MonadIO m => Hazard -> SpockT m ()
hazardWeb hazard = hazardWeb' hazard (evalRandIO makePassword) (evalRandIO newDeck)


hazardWeb' :: MonadIO m => Hazard -> IO ByteString -> IO (Deck Complete) -> SpockT m ()
hazardWeb' hazard pwgen deckGen = do
  get root View.home

  get Route.games $ do
    games' <- liftIO $ atomically $ getGames hazard
    View.games games'

  post Route.games $ withAuth (users hazard) $ \creator -> do
    gameRequest <- expectJSON
    case validateCreationRequest gameRequest of
     Left e -> View.badRequest (show e)
     Right r -> do
       (gameId, game) <- liftIO $ atomically $ createGame hazard creator r
       setStatus created201
       setHeader "Location" (renderRoute Route.game gameId)
       json game

  get Route.game $ \gameId -> do
    game <- liftIO $ atomically $ getGameSlot hazard gameId
    case game of
     Just game' -> View.game gameId game'
     Nothing -> View.errorMessage notFound404 ("no such game" :: Text)

  post Route.game $ \gameId -> withAuth (users hazard) $ \joiner -> do
    deck <- liftIO deckGen
    result <- liftIO $ atomically $ applySlotAction hazard gameId (joinSlot deck joiner)
    case result of
     Left (GameNotFound _) -> View.errorMessage notFound404 ("no such game" :: Text)
     Left (OtherError AlreadyStarted) -> View.badRequest ("Game already started" :: Text)
     Left (OtherError AlreadyFinished) -> View.badRequest ("Game already finished" :: Text)
     Left (OtherError (InvalidPlayers e)) -> View.internalError e
     Right (_, game) -> json game

  get Route.round $ \gameId roundId -> do
    viewer <- maybeLoggedInUser (users hazard)
    round <- liftIO $ atomically $ getRound hazard gameId roundId
    case round of
     Nothing -> View.errorMessage notFound404 ("no such round" :: Text)
     Just round' -> json (roundToJSON viewer round')

  post Route.round $ \gameId roundId ->
    withAuth (users hazard) $ \poster -> do
    playRequest <- expectJSON
    deck <- liftIO deckGen
    result <- liftIO $ atomically $ runEitherT $ do
      slot <- tryGetSlot hazard gameId
      -- TODO: Use types to enforce validated play requests
      let validation = validatePlayRequest poster roundId playRequest
      playRequest' <- hoistEither $ fst <$> runSlotAction validation slot
      result <- lift $ applySlotAction hazard gameId (playSlot deck playRequest')
      hoistEither $ fst <$> result

    case result of
     Left (GameNotFound {}) ->
       View.errorMessage notFound404 ("no such game" :: Text)
     Left (OtherError (RoundNotFound {})) ->
       View.errorMessage notFound404 ("no such round" :: Text)
     Left (OtherError (NotYourTurn _ current)) -> do
       setStatus badRequest400
       json (object ["message" .= ("Not your turn" :: Text),
                     "currentPlayer" .= current])
     Left (OtherError (NotInGame _)) -> do
       setStatus badRequest400
       json (object ["message" .= ("You are not playing" :: Text)])
     Left (OtherError (RoundNotActive {})) -> do
       setStatus badRequest400
       json (object ["message" .= ("Round not active" :: Text)])
     Left (OtherError e) -> do
       setStatus badRequest400
       json (object ["message" .= show e])
     Right result' -> json result'


  userWeb (users hazard) pwgen
