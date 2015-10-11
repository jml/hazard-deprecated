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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Hazard.HttpAuth ( Auth
                       , Credentials(..)
                       , PasswordAuth
                       , PasswordProtected
                       , maybeLoggedIn
                       , protectWith
                       ) where

import BasicPrelude

import Network.HTTP.Types (status401)
import Network.Wai (Request, Response, requestHeaders, responseLBS)
import Network.Wai.Middleware.HttpAuth

import Servant
import Servant.Server.Internal (succeedWith)


maybeLoggedIn :: Request -> Maybe ByteString
maybeLoggedIn req = do
  authHeader <- lookup "Authorization" $ requestHeaders req
  (u, _) <- extractBasicAuth authHeader
  return u


-- Abstract HTTP authentioation

data Auth creds user

data AuthProtected creds user handlers = AP
  { checkCreds :: creds -> IO (Maybe user)
  , onMissingAuthData :: IO Response
  , onCheckFailed :: IO Response
  , extractCreds :: Request -> Maybe creds
  , protectedHandlers :: handlers
  }

instance HasServer api => HasServer (Auth creds user :> api) where
  type ServerT (Auth creds user :> api) m =
    AuthProtected creds user (user -> ServerT api m)

  route _ authprotected req resp =
    case extractCreds authprotected req of
      Nothing -> onMissingAuthData authprotected >>= resp . succeedWith
      Just creds  -> do
        user <- checkCreds authprotected creds
        case user of
          Nothing -> onCheckFailed authprotected >>= resp . succeedWith
          Just user' -> route (Proxy :: Proxy api)
                              (protectedHandlers authprotected user')
                              req
                              resp

-- HTTP Basic authentioation

type PasswordAuth = Auth Credentials

type PasswordProtected = AuthProtected Credentials

data Credentials = Credentials { username :: ByteString
                               , password :: ByteString
                               }


extractCredentials :: Request -> Maybe Credentials
extractCredentials req = do
  header <- lookup "Authorization" (requestHeaders req)
  uncurry Credentials <$> extractBasicAuth header


protectWith :: Text
            -> (Credentials -> IO (Maybe user))
            -> handlers
            -> AuthProtected Credentials user handlers
protectWith realm check = AP check onMissing checkFailed extractCredentials
  where
    onMissing   = return authFailure
    checkFailed = return authFailure
    authFailure = responseLBS status401 [("WWW-Authenticate", headerBytes)] ""
    headerBytes = encodeUtf8 $ "Basic realm=\"" ++ realm ++ "\""
