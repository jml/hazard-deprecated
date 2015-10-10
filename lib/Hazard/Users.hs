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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hazard.Users ( UserDB
                    , UserID
                    , User
                    , UserCreationRequest
                    , addUser
                    , authenticate
                    , getUserByID
                    , getUserIDByName
                    , makeUserDB
                    , makePassword
                    , getAllUsers
                    , toJSONKey
                    ) where

import BasicPrelude

import Control.Concurrent.STM (TVar, newTVar, readTVar, writeTVar)
import Control.Error hiding ((!?))
import Control.Monad.STM (STM)
import Control.Monad.Random (uniform, MonadRandom)
import qualified Data.ByteString as B
import Data.Vector ((!?))
import qualified Data.Vector as V
import Web.PathPieces (PathPiece)

import Data.Aeson (FromJSON(..), ToJSON(..), (.=), Value(Object, String), object, (.:))
import Test.Tasty.QuickCheck (Arbitrary, arbitrary)


-- TODO: Stored as username / password. Password is in the clear, which is
-- terrible.  jml/hazard#4
data User = User {
  _userID :: UserID,
  _username :: ByteString,
  _password :: ByteString
  }


instance ToJSON User where
  toJSON user = object [ "username" .= decodeUtf8 (_username user)
                       , "id" .= _userID user
                       ]


newtype UserID = UserID Int deriving (Eq, Ord, Show, PathPiece)


instance ToJSON UserID where
  toJSON (UserID i) = toJSON (show i)


instance FromJSON UserID where
  parseJSON (String userId) = UserID <$> (readZ . textToString $ userId)
  parseJSON _ = mzero


toJSONKey :: UserID -> Text
toJSONKey (UserID i) = show i


instance Arbitrary UserID where
  arbitrary = UserID <$> arbitrary


data UserCreationRequest = UserCreationRequest { reqUsername :: Text }

instance ToJSON UserCreationRequest where
  toJSON req = object ["username" .= reqUsername req]

instance FromJSON UserCreationRequest where
  parseJSON (Object v) = UserCreationRequest <$> v .: "username"
  parseJSON _ = mzero


newtype UserDB = UserDB { unUserDB :: TVar (Vector User) }


makeUserDB :: STM UserDB
makeUserDB = UserDB <$> newTVar empty


getAllUsers :: UserDB -> STM [(UserID, ByteString)]
getAllUsers db = do
  users <- readTVar (unUserDB db)
  return [(_userID user, _username user) | user <- V.toList users]


getUserByID :: UserDB -> UserID -> STM (Maybe User)
getUserByID userDB (UserID i) = do
  allUsers <- readTVar (unUserDB userDB)
  return $ allUsers !? i


getUserByName :: UserDB -> ByteString -> STM (Maybe User)
getUserByName userDB username = do
  allUsers <- readTVar (unUserDB userDB)
  return $ find ((== username) . _username) allUsers


getUserIDByName :: UserDB -> ByteString -> STM (Maybe UserID)
getUserIDByName userDB username = fmap _userID <$> getUserByName userDB username


authenticate :: UserDB -> ByteString -> ByteString -> STM (Maybe User)
authenticate userDB username password = do
  foundUser <- getUserByName userDB username
  return $ case foundUser of
   Nothing -> Nothing
   Just user
     | _password user == password -> Just user
     | otherwise -> Nothing


addUser :: UserDB -> UserCreationRequest -> ByteString -> STM (Maybe UserID)
addUser userDB req password =
  let username = encodeUtf8 (reqUsername req)
      users' = unUserDB userDB
  in do
    existing <- getUserByName userDB username
    case existing of
     Just _ -> return Nothing
     Nothing  -> do
       allUsers <- readTVar users'
       let newID = UserID (length allUsers)
       let newUser = User newID username password
       writeTVar users' (V.snoc allUsers newUser)
       return $ Just newID


makePassword :: MonadRandom m => m ByteString
makePassword = B.pack <$> replicateM passwordLength randomPasswordChar
  where passwordLength = 16
        randomPasswordChar = uniform passwordChars
        passwordChars = B.unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
