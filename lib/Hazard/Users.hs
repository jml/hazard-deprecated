{-# LANGUAGE OverloadedStrings #-}

module Hazard.Users ( UserDB
                    , addUser
                    , makeUserDB
                    , makePassword
                    , usernames
                    ) where

import Control.Concurrent.STM (TVar, newTVar, readTVar, writeTVar)
import Control.Monad (mzero, replicateM)
import Control.Monad.STM (STM)
import Control.Monad.Random (Rand, uniform)
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import System.Random (RandomGen)

import Data.Aeson (FromJSON(..), ToJSON(..), (.=), Value(Object), object, (.:))


data User = User B.ByteString B.ByteString

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

usernames :: UserDB -> STM [B.ByteString]
usernames = fmap (map username) . readTVar . unUserDB
            where username (User u _) = u


addUser :: UserDB -> UserCreationRequest -> B.ByteString -> STM (Maybe Int)
addUser userDB req password =
  let username = encodeUtf8 (reqUsername req)
      newUser = User username password
      users' = unUserDB userDB
  in do
     allUsers <- readTVar users'
     case filter (\(User u _) -> u == username) allUsers of
      [] -> do
        writeTVar users' (newUser:allUsers)
        return $ Just $ length allUsers
      _ -> return Nothing


makePassword :: RandomGen g => Rand g B.ByteString
makePassword = B.pack <$> replicateM passwordLength randomPasswordChar
  where passwordLength = 16
        randomPasswordChar = uniform passwordChars
        passwordChars = B.unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
