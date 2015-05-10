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

{-# LANGUAGE OverloadedStrings #-}

module Hazard.HttpAuth (maybeLoggedIn) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as B
import Network.Wai (Request, requestHeaders)
import Network.Wai.Middleware.HttpAuth
import Web.Spock.Safe


maybeLoggedIn :: Request -> Maybe B.ByteString
maybeLoggedIn req = do
  authHeader <- lookup "Authorization" $ requestHeaders req
  (u, _) <- extractBasicAuth authHeader
  return u


getLoggedIn :: MonadIO m => ActionT m B.ByteString
getLoggedIn = do
  req <- request
  case maybeLoggedIn req of
   Nothing -> error "Should be logged in, but not"
   Just u -> return u
