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

import Control.Concurrent.STM (TVar, newTVar, readTVar, writeTVar)
import Control.Monad (mzero, replicateM)
import Control.Monad.STM (STM)
import Control.Monad.Random (Rand, uniform)
import qualified Data.ByteString.Lazy as B
import Data.Foldable (find)
import Data.List (findIndex)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import System.Random (RandomGen)

import Data.Aeson (FromJSON(..), ToJSON(..), (.=), Value(Object), object, (.:))


-- XXX: Stored as username / password. Password is in the clear, which is
-- terrible.
data User = User B.ByteString B.ByteString

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

usernames :: UserDB -> STM [B.ByteString]
usernames = fmap (map username) . readTVar . unUserDB
            where username (User u _) = u


getUserByID :: UserDB -> Int -> STM (Maybe User)
getUserByID userDB i = do
  allUsers <- readTVar (unUserDB userDB)
  return $ if 0 <= i && i < length allUsers then Just (allUsers !! i) else Nothing


getUserByName :: UserDB -> B.ByteString -> STM (Maybe User)
getUserByName userDB username = do
  allUsers <- readTVar (unUserDB userDB)
  return $ find (\(User u _) -> u == username) allUsers


getUserIDByName :: UserDB -> B.ByteString -> STM (Maybe Int)
getUserIDByName userDB username = do
  allUsers <- readTVar (unUserDB userDB)
  return $ findIndex (\(User u _) -> u == username) allUsers



authenticate :: UserDB -> B.ByteString -> B.ByteString -> STM (Maybe User)
authenticate userDB username password = do
  foundUser <- getUserByName userDB username
  return $ case foundUser of
   Nothing -> Nothing
   Just u@(User _ p)
     | p == password -> Just u
     | otherwise -> Nothing


addUser :: UserDB -> UserCreationRequest -> B.ByteString -> STM (Maybe Int)
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


makePassword :: RandomGen g => Rand g B.ByteString
makePassword = B.pack <$> replicateM passwordLength randomPasswordChar
  where passwordLength = 16
        randomPasswordChar = uniform passwordChars
        passwordChars = B.unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
