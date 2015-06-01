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
                    , UserID
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
import Control.Monad.STM (STM)
import Control.Monad.Random (Rand, uniform)
import qualified Data.ByteString as B
import Data.Vector ((!?))
import qualified Data.Vector as V
import System.Random (RandomGen)

import Data.Aeson (FromJSON(..), ToJSON(..), (.=), Value(Object), object, (.:))


-- TODO: Stored as username / password. Password is in the clear, which is
-- terrible.  jml/hazard#4
data User = User ByteString ByteString

type UserID = Int


instance ToJSON User where
  toJSON (User u _) = object ["username" .= decodeUtf8 u]

data UserCreationRequest = UserCreationRequest { reqUsername :: Text }

instance ToJSON UserCreationRequest where
  toJSON req = object ["username" .= reqUsername req]

instance FromJSON UserCreationRequest where
  parseJSON (Object v) = UserCreationRequest <$> v .: "username"
  parseJSON _ = mzero


newtype UserDB = UserDB { unUserDB :: TVar (Vector User) }


makeUserDB :: STM UserDB
makeUserDB = UserDB <$> newTVar empty

usernames :: UserDB -> STM (Vector ByteString)
usernames = fmap (map username) . readTVar . unUserDB
            where username (User u _) = u


getUserByID :: UserDB -> UserID -> STM (Maybe User)
getUserByID userDB i = do
  allUsers <- readTVar (unUserDB userDB)
  return $ allUsers !? i


getUserByName :: UserDB -> ByteString -> STM (Maybe User)
getUserByName userDB username = do
  allUsers <- readTVar (unUserDB userDB)
  return $ find (\(User u _) -> u == username) allUsers


getUserIDByName :: UserDB -> ByteString -> STM (Maybe UserID)
getUserIDByName userDB username = do
  allUsers <- readTVar (unUserDB userDB)
  return $ V.findIndex (\(User u _) -> u == username) allUsers


authenticate :: UserDB -> ByteString -> ByteString -> STM (Maybe User)
authenticate userDB username password = do
  foundUser <- getUserByName userDB username
  return $ case foundUser of
   Nothing -> Nothing
   Just u@(User _ p)
     | p == password -> Just u
     | otherwise -> Nothing


addUser :: UserDB -> UserCreationRequest -> ByteString -> STM (Maybe UserID)
addUser userDB req password =
  let username = encodeUtf8 (reqUsername req)
      newUser = User username password
      users' = unUserDB userDB
  in do
     allUsers <- readTVar users'
     case V.find (\(User u _) -> u == username) allUsers of
      Nothing  -> do
        writeTVar users' (V.snoc allUsers newUser)
        return $ Just $ length allUsers
      _ -> return Nothing


makePassword :: RandomGen g => Rand g ByteString
makePassword = B.pack <$> replicateM passwordLength randomPasswordChar
  where passwordLength = 16
        randomPasswordChar = uniform passwordChars
        passwordChars = B.unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
