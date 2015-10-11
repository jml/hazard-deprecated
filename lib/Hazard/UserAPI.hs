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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Hazard.UserAPI (PasswordGenerator, UserAPI, userAPI, server) where

import BasicPrelude

import Control.Concurrent.STM (STM, atomically)
import Control.Monad.Except (throwError)
import Control.Monad.Random (MonadRandom, evalRandIO)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader

import Data.Aeson (ToJSON(..), (.=), object)
import Data.ByteString.Conversion.To (ToByteString(..))
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze (ToMarkup(..), (!))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (href)

import Hazard.Users (
  UserCreationRequest,
  User,
  UserDB,
  UserID,
  addUser,
  getAllUsers,
  getUserID,
  getUserByID,
  getUsername,
  )


-- TODO:  Next steps:
--
-- * make sure the output is the same as the other version
-- * run tests against this
--   * maybe the same tests as the Spock ones?
--
-- Bugs
-- * Broken link on "user" page (goes to /user/user/0 rather than /user/0)
-- * Password not returned on user creation
-- * Documentation missing when browsing on HTML
-- * /users includes ID and username, rather than just user name
--
-- Ugliness
-- * PublicUser type should probably be phantom type on User
-- * Linking to users has a bunch of ugly code that will need to be repeated


type UserAPI = "users" :> Get '[JSON, HTML] [PublicUser]
               :<|> "users" :> ReqBody '[JSON] UserCreationRequest :> Post '[JSON] (Headers '[Header "Location" URI] User)
               :<|> "user" :> Capture "userID" UserID :> Get '[JSON, HTML] PublicUser


userAPI :: Proxy UserAPI
userAPI = Proxy


userLink :: Proxy ("user" :> Capture "userID" UserID :> Get '[HTML] PublicUser)
userLink = Proxy


newtype PublicUser = PublicUser (UserID, Text)

publicUser :: User -> PublicUser
publicUser user = PublicUser (getUserID user, decodeUtf8 (getUsername user))


instance ToMarkup PublicUser where

  toMarkup (PublicUser (userID, username)) = H.a ! href (H.toValue (show (safeLink userAPI userLink userID))) $ H.text username


instance ToMarkup [PublicUser] where

  toMarkup users = H.ul $ mapM_ (H.li . toMarkup) users


instance ToJSON PublicUser where
  toJSON (PublicUser (userID, username)) = object [ "username" .= username
                                                  , "id" .= userID
                                                  ]


instance ToByteString URI where
  builder = builder . show


serverT :: ServerT UserAPI UserHandler
serverT = allUsers :<|> makeUser :<|> oneUser


server :: UserDB -> PasswordGenerator -> Server UserAPI
server userDB pwgen = enter (readerToEither userDB pwgen) serverT


type UserHandler = ReaderT (UserDB, ByteString) (EitherT ServantErr STM)

type PasswordGenerator = forall m. MonadRandom m => m ByteString


getDatabase :: UserHandler UserDB
getDatabase = fst <$> ask


generatePassword :: UserHandler ByteString
generatePassword = snd <$> ask


allUsers :: UserHandler [PublicUser]
allUsers = do
  userDB <- getDatabase
  lift $ lift $ map PublicUser <$> map (second decodeUtf8) <$> getAllUsers userDB


makeUser :: UserCreationRequest -> UserHandler (Headers '[Header "Location" URI] User)
makeUser userRequest = do
  userDB <- getDatabase
  password <- generatePassword
  user <- lift $ lift $ addUser userDB userRequest password
  case user of
    Just user' -> return $ addHeader (linkToUser user') user'
    Nothing -> throwError $ err400 { errBody = "username already exists" }
  where
    -- XXX: Duplicated
    linkToUser u = safeLink userAPI userLink (getUserID u)


oneUser :: UserID -> UserHandler PublicUser
oneUser userID = do
  userDB <- getDatabase
  user <- lift $ lift $ getUserByID userDB userID
  case user of
    Just user' -> return (publicUser user')
    Nothing -> throwError $ err404 { errBody = "no such user" }


readerToEither :: UserDB -> PasswordGenerator -> UserHandler :~> EitherT ServantErr IO
readerToEither userDB pwgen = Nat (readerToEither' userDB pwgen)


readerToEither' :: UserDB -> PasswordGenerator -> UserHandler a -> EitherT ServantErr IO a
readerToEither' userDB pwgen action = do
  -- XXX: Assumes we only need to generate a password once per user request.
  -- I'm pretty sure the only alternative is to do unsafePerformIO for the
  -- password generation, or just give up and do the whole action in IO.
  password <- liftIO $ evalRandIO pwgen
  result <- liftIO $ atomically (runEitherT (runReaderT action (userDB, password)))
  hoistEither result

