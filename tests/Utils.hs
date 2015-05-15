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

module Utils ( hazardTestApp
             , getAs
             , postAs
             ) where

import Control.Monad.STM (atomically)

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64

import Network.HTTP.Types.Header (Header)
import Network.Wai (Application)
import Test.Hspec.Wai
import Web.Spock.Safe (spockAsApp, spockT)

import Hazard (hazardWeb', makeHazard)


hazardTestApp :: IO Application
hazardTestApp = do
  hazard <- atomically makeHazard
  spockAsApp $ spockT id $ hazardWeb' hazard (return "password")


getAs username url = request "GET" url [authHeader username "password"] ""

postAs username url = request "POST" url [authHeader username "password"]

authHeader :: B.ByteString -> B.ByteString -> Header
authHeader username password = ("Authorization", B.concat ["Basic ", encodedCredentials])
  where encodedCredentials = Base64.encode $ B.concat [username, ":", password]
