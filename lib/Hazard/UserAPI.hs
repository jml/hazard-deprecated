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
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Hazard.UserAPI (userAPI, server) where

import BasicPrelude

import Control.Concurrent.STM (STM, atomically)
import Control.Monad.Random (MonadRandom, evalRandIO)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader

import Servant
import Servant.HTML.Blaze (HTML)

import Hazard.Users (
  UserCreationRequest,
  User,
  UserDB,
  UserID,
  addUser,
  getAllUsers,
  getUserByID,
  )


type UserAPI = "users" :> Get '[JSON, HTML] [(UserID, Text)]
               :<|> "users" :> ReqBody '[JSON] UserCreationRequest :> Post '[JSON] User
               :<|> "user" :> Capture "userID" UserID :> Get '[JSON, HTML] User


userAPI :: Proxy UserAPI
userAPI = Proxy


serverT :: ServerT UserAPI UserHandler
serverT = allUsers :<|> makeUser :<|> oneUser


server :: UserDB -> PasswordGenerator -> Server UserAPI
server userDB pwgen = enter (readerToEither userDB pwgen) serverT


type UserHandler = ReaderT (UserDB, ByteString) STM

type PasswordGenerator = forall m. MonadRandom m => m ByteString


getDatabase :: UserHandler UserDB
getDatabase = fst <$> ask


generatePassword :: UserHandler ByteString
generatePassword = snd <$> ask


allUsers :: UserHandler [(UserID, Text)]
allUsers = do
  userDB <- getDatabase
  lift $ map (second decodeUtf8) <$> getAllUsers userDB


makeUser :: UserCreationRequest -> UserHandler User
makeUser userRequest = do
  userDB <- getDatabase
  password <- generatePassword
  user <- lift $ addUser userDB userRequest password
  return $ fromMaybe (terror "user already exists") user


oneUser :: UserID -> UserHandler User
oneUser userID = do
  userDB <- getDatabase
  lift $ fromMaybe (terror "no such user") <$> getUserByID userDB userID


readerToEither :: UserDB -> PasswordGenerator -> UserHandler :~> EitherT ServantErr IO
readerToEither userDB pwgen = Nat (readerToEither' userDB pwgen)


readerToEither' :: UserDB -> PasswordGenerator -> UserHandler a -> EitherT ServantErr IO a
readerToEither' userDB pwgen action = do
  -- XXX: Assumes we only need to generate a password once per user request.
  -- I'm pretty sure the only alternative is to do unsafePerformIO for the
  -- password generation, or just give up and do the whole action in IO.
  password <- liftIO $ evalRandIO pwgen
  liftIO $ atomically (runReaderT action (userDB, password))


