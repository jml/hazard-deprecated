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
  performSlotAction,
  setGame,
  users
  )

import qualified Hazard.Games as Games
import Hazard.Games (
  createGame,
  JoinError(..),
  GameError(..),
  joinSlot,
  roundToJSON,
  validateCreationRequest)

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


errorMessage :: (MonadIO m, ToJSON a) => Status -> a -> ActionT m ()
errorMessage code message = do
  setStatus code
  json (object ["message" .= message])


badRequest :: (ToJSON a, MonadIO m) => a -> ActionT m ()
badRequest = errorMessage badRequest400


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

  middleware $ basicAuth (authUserDB userDB) ("Hazard API" { authIsProtected = isProtected })

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

  where isProtected req = return $ case (requestMethod req, pathInfo req) of
          -- XXX: Alter this to be default POST requires auth, except
          -- registering users, and all others don't need auth
          (_, "user":_) -> True
          ("POST", "games":_) -> True
          ("POST", "game":_) -> True
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


loggedInUser :: MonadIO m => UserDB -> ActionT m UserID
loggedInUser userDB = do
  maybeUser <- maybeLoggedInUser userDB
  case maybeUser of
   Just user -> return user
   -- XXX: Really ought to raise some kind of error.
   Nothing -> error "No user logged in"


hazardWeb :: MonadIO m => Hazard -> SpockT m ()
hazardWeb hazard = hazardWeb' hazard (evalRandIO makePassword)


hazardWeb' :: MonadIO m => Hazard -> IO ByteString -> SpockT m ()
hazardWeb' hazard pwgen = do
  get "/" $ html "Hello World!"

  get "/games" $ do
    games' <- liftIO $ atomically $ getGames hazard
    json ["/game/" ++ show i | i <- [0..length games' - 1]]

  post "/games" $ do
    creator <- loggedInUser (users hazard)
    gameRequest <- expectJSON
    case validateCreationRequest gameRequest of
     Left e -> terror $ show e  -- XXX: Should be bad request
     Right r -> do
       let newGame = createGame creator r
       liftIO $ atomically $ addGame hazard newGame
       setStatus created201
       setHeader "Location" "/game/0"
       json (Nothing :: Maybe Int)

  get ("game" <//> var) $ \gameId -> do
    game <- liftIO $ atomically $ getGameSlot hazard gameId
    case game of
     Just game' -> json game'
     Nothing -> errorMessage notFound404 ("no such game" :: Text)

  post ("game" <//> var) $ \gameId -> do
    joiner <- loggedInUser (users hazard)
    result <- liftIO $ performSlotAction hazard gameId (joinSlot joiner)
    case result of
     Left (GameNotFound _) -> errorMessage notFound404 ("no such game" :: Text)
     Left (JoinError AlreadyStarted) -> badRequest ("Game already started" :: Text)
     Left e -> terror $ show e
     Right (_, game) -> json game

  get ("game" <//> var <//> "round" <//> var) $ \gameId roundId -> do
    viewer <- maybeLoggedInUser (users hazard)
    round <- liftIO $ atomically $ getRound hazard gameId roundId
    case round of
     Nothing -> errorMessage notFound404 ("no such round" :: Text)
     Just round' -> json (roundToJSON viewer round')

  post ("game" <//> var <//> "round" <//> var) $ \gameId roundId -> do
    poster <- loggedInUser (users hazard)
    round <- liftIO $ atomically $ getRound hazard gameId roundId
    case round of
     Nothing -> errorMessage notFound404 ("no such round" :: Text)
     Just round'
       | poster `notElem` Games.getPlayers round' ->
           do setStatus badRequest400
              json (object ["message" .= ("You are not playing" :: Text)])
       | Just poster /= Games.currentPlayer round' ->
           do setStatus badRequest400
              json (object ["message" .= ("Not your turn" :: Text),
                            "currentPlayer" .= Games.currentPlayer round'])
       | otherwise ->
           do playRequest <- expectJSON
              game <- liftIO $ atomically $ getGameSlot hazard gameId
              case game of
               Nothing -> error "Found round but not game. WTF?"
               Just game' ->
                 case Games.playTurn game' playRequest of
                  Left e -> do
                    setStatus badRequest400
                    json (object ["message" .= show e])
                  Right (result, game'') -> do
                    liftIO $ atomically $ setGame hazard gameId game''
                    json result


  userWeb (users hazard) pwgen
