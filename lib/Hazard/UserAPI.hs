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
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader

import Servant
import Servant.HTML.Blaze (HTML)

import Hazard.Users (UserCreationRequest, User, UserDB, UserID, getAllUsers, getUserByID)


type UserAPI = "users" :> Get '[JSON, HTML] [(UserID, Text)]
               :<|> "users" :> ReqBody '[JSON] UserCreationRequest :> Post '[JSON] User
               :<|> "user" :> Capture "userID" UserID :> Get '[JSON, HTML] User


userAPI :: Proxy UserAPI
userAPI = Proxy


serverT :: ServerT UserAPI UserHandler
serverT = allUsers :<|> addUser :<|> oneUser


server :: UserDB -> Server UserAPI
server userDB = enter (readerToEither userDB) serverT


type UserHandler = ReaderT UserDB STM


allUsers :: UserHandler [(UserID, Text)]
allUsers = do
  userDB <- ask
  lift $ map (second decodeUtf8) <$> getAllUsers userDB


addUser :: UserCreationRequest -> m User
addUser = undefined


oneUser :: UserID -> UserHandler User
oneUser userID = do
  userDB <- ask
  lift $ fromMaybe (terror "no such user") <$> getUserByID userDB userID


readerToEither :: UserDB -> UserHandler :~> EitherT ServantErr IO
readerToEither userDB = Nat (readerToEither' userDB)


readerToEither' :: UserDB -> UserHandler a -> EitherT ServantErr IO a
readerToEither' userDB action = liftIO $ atomically (runReaderT action userDB)


