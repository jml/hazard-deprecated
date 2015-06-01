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

import Data.Aeson (FromJSON(..), ToJSON(..), (.=), object)
import Network.HTTP.Types.Status
import Network.Wai (requestMethod, pathInfo)
import Network.Wai.Middleware.HttpAuth
import Web.Spock.Safe


import Haverer.Internal.Error

import Hazard.HttpAuth (maybeLoggedIn)

import Hazard.Model (
  Hazard,
  addGame,
  getGameSlot,
  getGames,
  getRound,
  makeHazard,
  applySlotAction',
  performSlotAction,
  runSlotAction',
  tryGetSlot,
  users
  )

import Hazard.Games (
  createGame,
  JoinError(..),
  GameError(..),
  PlayError(..),
  joinSlot,
  playSlot,
  roundToJSON,
  validateCreationRequest,
  validatePlayRequest
  )

import Hazard.Users (
  UserDB,
  UserID,
  addUser,
  authenticate,
  getUserByID,
  getUserIDByName,
  makePassword,
  usernames
  )


realm :: Text
realm = "Hazard API"

errorMessage :: (MonadIO m, ToJSON a) => Status -> a -> ActionT m ()
errorMessage code message = do
  setStatus code
  json (object ["message" .= message])


badRequest :: (ToJSON a, MonadIO m) => a -> ActionT m ()
badRequest = errorMessage badRequest400


authenticationRequired :: MonadIO m => ActionT m ()
authenticationRequired = do
  setHeader "WWW-Authenticate" realm
  errorMessage unauthorized401 ("Must log in" :: Text)


expectJSON :: (MonadIO m, FromJSON a) => ActionT m a
expectJSON = do
  body' <- jsonBody
  case body' of
   Nothing -> do
     setStatus badRequest400
     text "Expected JSON, but could not parse it"
   Just contents -> return contents


userWeb :: MonadIO m => UserDB -> IO ByteString -> SpockT m ()
userWeb userDB pwgen = do

  middleware $ basicAuth (authUserDB userDB) ("" { authRealm = encodeUtf8 realm,
                                                   authIsProtected = isProtected })

  get "/users" $ do
    usernames' <- liftIO $ atomically $ usernames userDB
    json $ map decodeUtf8 usernames'

  post "/users" $ do
    userRequest <- expectJSON
    password <- liftIO pwgen
    newID <- liftIO $ atomically $ addUser userDB userRequest password
    case newID of
     Just newID' -> do
       setStatus created201
       setHeader "Location" ("/user/" ++ show newID')
       json (object ["password" .= decodeUtf8 password])
     Nothing -> badRequest ("username already exists" :: Text)

  get ("user" <//> var) $ \userID -> do
    user <- liftIO $ atomically $ getUserByID userDB userID
    case user of
     Just user' -> json user'
     Nothing -> errorMessage notFound404 ("no such user" :: Text)

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
withAuth userDB action = maybeLoggedInUser userDB >>= maybe authenticationRequired action


hazardWeb :: MonadIO m => Hazard -> SpockT m ()
hazardWeb hazard = hazardWeb' hazard (evalRandIO makePassword)


hazardWeb' :: MonadIO m => Hazard -> IO ByteString -> SpockT m ()
hazardWeb' hazard pwgen = do
  get "/" $ html "Hello World!"

  get "/games" $ do
    games' <- liftIO $ atomically $ getGames hazard
    json ["/game/" ++ show i | i <- [0..length games' - 1]]

  post "/games" $ withAuth (users hazard) $ \creator -> do
    gameRequest <- expectJSON
    case validateCreationRequest gameRequest of
     Left e -> badRequest (show e)
     Right r -> do
       let newGame = createGame creator r
       (gameId, game) <- liftIO $ atomically $ addGame hazard newGame
       setStatus created201
       setHeader "Location" ("/game/" ++ show gameId)
       json game

  get ("game" <//> var) $ \gameId -> do
    game <- liftIO $ atomically $ getGameSlot hazard gameId
    case game of
     Just game' -> json game'
     Nothing -> errorMessage notFound404 ("no such game" :: Text)

  post ("game" <//> var) $ \gameId -> withAuth (users hazard) $ \joiner -> do
    result <- liftIO $ performSlotAction hazard gameId (joinSlot joiner)
    case result of
     Left (GameNotFound _) -> errorMessage notFound404 ("no such game" :: Text)
     Left (OtherError AlreadyStarted) -> badRequest ("Game already started" :: Text)
     Left e -> terror $ show e
     Right (_, game) -> json game

  get ("game" <//> var <//> "round" <//> var) $ \gameId roundId -> do
    viewer <- maybeLoggedInUser (users hazard)
    round <- liftIO $ atomically $ getRound hazard gameId roundId
    case round of
     Nothing -> errorMessage notFound404 ("no such round" :: Text)
     Just round' -> json (roundToJSON viewer round')

  post ("game" <//> var <//> "round" <//> var) $ \gameId roundId ->
    withAuth (users hazard) $ \poster -> do
    playRequest <- expectJSON

    result <- liftIO $ atomically $ runEitherT $ do
      slot <- tryGetSlot hazard gameId
      -- TODO: Use types to enforce validated play requests
      let validation = validatePlayRequest poster roundId playRequest
      playRequest' <- hoistEither $ fst <$> runSlotAction' validation slot
      result <- lift $ applySlotAction' hazard gameId (playSlot playRequest')
      hoistEither $ fst <$> result

    case result of
     Left (GameNotFound {}) ->
       errorMessage notFound404 ("no such game" :: Text)
     Left (OtherError (RoundNotFound {})) ->
       errorMessage notFound404 ("no such round" :: Text)
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
