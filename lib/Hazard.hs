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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Hazard ( Hazard
              , hazardWeb
              , hazardWeb'
              , makeHazard
              ) where


import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Random (evalRandIO)
import Control.Monad.STM (STM, atomically)
import Control.Concurrent.STM (TVar, newTVar, readTVar, writeTVar)

import Data.Aeson (FromJSON(..), ToJSON(..), (.=), Value(Object), object, (.:))
import qualified Data.ByteString as B
import Data.Maybe (isJust)
import Data.Text (Text, append, pack)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types.Status
import Network.Wai (requestMethod, pathInfo)
import Network.Wai.Middleware.HttpAuth
import Web.Spock.Safe


import Hazard.HttpAuth (maybeLoggedIn)

import Hazard.Model (
  createGame,
  GameCreationRequest(..),
  GameSlot,
  requestGame,
  Validated(Unchecked),
  validateCreationRequest)

import Hazard.Users (
  UserDB,
  addUser,
  authenticate,
  getUserByID,
  getUserIDByName,
  makePassword,
  makeUserDB,
  usernames
  )


instance ToJSON (GameCreationRequest a) where
  toJSON r = object [ "numPlayers" .= reqNumPlayers r
                    , "turnTimeout" .= reqTurnTimeout r
                    ]

instance FromJSON (GameCreationRequest 'Unchecked) where
  parseJSON (Object v) = requestGame <$> v .: "numPlayers" <*> v .: "turnTimeout"
  parseJSON _ = mzero


data Hazard = Hazard { games :: TVar [GameSlot Int]
                     , users :: UserDB
                     }


makeHazard :: STM Hazard
makeHazard = Hazard <$> newTVar [] <*> makeUserDB


getGames :: Hazard -> STM [GameSlot Int]
getGames = readTVar . games


getGame :: Hazard -> Int -> STM (Maybe (GameSlot Int))
getGame hazard i = do
  games' <- readTVar (games hazard)
  if 0 <= i && i < length games'
    then return $ Just (games' !! i)
    else return Nothing


addGame :: Hazard -> GameSlot Int -> STM ()
addGame hazard game = do
  let allGames = games hazard
  games' <- readTVar allGames
  writeTVar allGames (games' ++ [game])


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


userWeb :: MonadIO m => UserDB -> IO B.ByteString -> SpockT m ()
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
       setHeader "Location" (append "/user/" (pack (show newID')))
       json (object ["password" .= decodeUtf8 password])
     Nothing -> badRequest ("username already exists" :: Text)

  get ("user" <//> var) $ \userID -> do
    user <- liftIO $ atomically $ getUserByID userDB userID
    case user of
     Just user' -> json user'
     Nothing -> errorMessage notFound404 ("no such user" :: Text)

  where isProtected req = return $ case (requestMethod req, pathInfo req) of
          (_, "user":_) -> True
          ("POST", "games":_) -> True
          _ -> False


authUserDB :: UserDB -> CheckCreds
authUserDB userDB username password = do
  found <- liftIO $ atomically $ authenticate userDB username password
  return (isJust found)


maybeLoggedInUser :: MonadIO m => UserDB -> ActionT m (Maybe Int)
maybeLoggedInUser userDB = do
  req <- request
  case maybeLoggedIn req of
   Just user -> liftIO $ atomically $ getUserIDByName userDB user
   Nothing -> return Nothing


loggedInUser :: MonadIO m => UserDB -> ActionT m Int
loggedInUser userDB = do
  maybeUser <- maybeLoggedInUser userDB
  case maybeUser of
   Just user -> return user
   -- XXX: Really ought to raise some kind of error.
   Nothing -> error "No user logged in"


hazardWeb :: MonadIO m => Hazard -> SpockT m ()
hazardWeb hazard = hazardWeb' hazard (evalRandIO makePassword)


hazardWeb' :: MonadIO m => Hazard -> IO B.ByteString -> SpockT m ()
hazardWeb' hazard pwgen = do
  get "/" $ html "Hello World!"
  get "/games" $ do
    games' <- liftIO $ atomically $ getGames hazard
    json ["/game/" ++ show i | i <- [0..length games' - 1]]
  post "/games" $ do
    creator <- loggedInUser (users hazard)
    gameRequest <- expectJSON
    case validateCreationRequest gameRequest of
     Left e -> error $ show e  -- XXX: Should be bad request
     Right r -> do
       let newGame = createGame creator r
       liftIO $ atomically $ addGame hazard newGame
       setStatus created201
       setHeader "Location" "/game/0"
       json (Nothing :: Maybe Int)
  get ("game" <//> var) $ \gameId -> do
    game <- liftIO $ atomically $ getGame hazard gameId
    json game

  userWeb (users hazard) pwgen
