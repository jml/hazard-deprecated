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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hazard.Users ( UserDB
                    , addUser
                    , authenticate
                    , getUserByID
                    , getUserIDByName
                    , makeUserDB
                    , makePassword
                    , usernames
                    ) where

import BasicPrelude

import Control.Concurrent.STM (TVar, newTVar, readTVar, writeTVar)
import Control.Error
import Control.Monad.STM (STM)
import Control.Monad.Random (Rand, uniform)
import qualified Data.ByteString as B
import System.Random (RandomGen)

import Data.Aeson (FromJSON(..), ToJSON(..), (.=), Value(Object), object, (.:))


-- XXX: Stored as username / password. Password is in the clear, which is
-- terrible.
data User = User ByteString ByteString

instance ToJSON User where
  toJSON (User u _) = object ["username" .= decodeUtf8 u]

data UserCreationRequest = UserCreationRequest { reqUsername :: Text }

instance ToJSON UserCreationRequest where
  toJSON req = object ["username" .= reqUsername req]

instance FromJSON UserCreationRequest where
  parseJSON (Object v) = UserCreationRequest <$> v .: "username"
  parseJSON _ = mzero


newtype UserDB = UserDB { unUserDB :: TVar [User] }

-- XXX: I feel like there's a monad-transformer way of writing user db that
-- would make it more composeable.

makeUserDB :: STM UserDB
makeUserDB = UserDB <$> newTVar []

usernames :: UserDB -> STM [ByteString]
usernames = fmap (map username) . readTVar . unUserDB
            where username (User u _) = u


getUserByID :: UserDB -> Int -> STM (Maybe User)
getUserByID userDB i = do
  allUsers <- readTVar (unUserDB userDB)
  return $ atMay allUsers i


getUserByName :: UserDB -> ByteString -> STM (Maybe User)
getUserByName userDB username = do
  allUsers <- readTVar (unUserDB userDB)
  return $ find (\(User u _) -> u == username) allUsers


getUserIDByName :: UserDB -> ByteString -> STM (Maybe Int)
getUserIDByName userDB username = do
  allUsers <- readTVar (unUserDB userDB)
  return $ findIndex (\(User u _) -> u == username) allUsers



authenticate :: UserDB -> ByteString -> ByteString -> STM (Maybe User)
authenticate userDB username password = do
  foundUser <- getUserByName userDB username
  return $ case foundUser of
   Nothing -> Nothing
   Just u@(User _ p)
     | p == password -> Just u
     | otherwise -> Nothing


addUser :: UserDB -> UserCreationRequest -> ByteString -> STM (Maybe Int)
addUser userDB req password =
  let username = encodeUtf8 (reqUsername req)
      newUser = User username password
      users' = unUserDB userDB
  in do
     allUsers <- readTVar users'
     case filter (\(User u _) -> u == username) allUsers of
      [] -> do
        writeTVar users' (allUsers ++ [newUser])
        return $ Just $ length allUsers
      _ -> return Nothing


makePassword :: RandomGen g => Rand g ByteString
makePassword = B.pack <$> replicateM passwordLength randomPasswordChar
  where passwordLength = 16
        randomPasswordChar = uniform passwordChars
        passwordChars = B.unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
