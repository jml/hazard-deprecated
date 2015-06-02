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

module Hazard.Views (
  realm
    -- | Pages
  , home
    -- | Errors
  , authenticationRequired
  , badRequest
  , errorMessage
  , internalError
  ) where

import BasicPrelude

import Data.Aeson (ToJSON, (.=), object)
import qualified Data.Text as Text
import Network.HTTP.Types.Status
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Web.Spock.Safe (
  ActionT,
  ClientPreferredFormat(..),
  lazyBytes,
  json,
  preferredFormat,
  setStatus,
  setHeader
  )


dualResponse :: (ToJSON a, MonadIO m) => a -> H.Html -> ActionT m b
dualResponse j h = do
  format <- preferredFormat
  case format of
   PrefJSON -> json j
   _ -> lazyBytes . renderHtml $ h

realm :: Text
realm = "Hazard API"


type View m = ActionT m ()


home :: MonadIO m => View m
home =
  dualResponse jsonView htmlView
  where
    jsonView = object ["message" .= ("Hazard API" :: Text)]
    htmlView = H.docTypeHtml $ do
      H.head $ H.title "Hazard - A dangerous card game"
      H.body $ do
        H.h1 "Hazard"
        H.p "An implementation of a card game you know and love."
        H.p "This is currently intended as an API server, rather than an \
            \ interactive user interface."
        H.p "The API will change."
        H.h2 "Source code"
        H.ul $ do
              H.li $ do
                H.a ! href "https://github.com/jml/hazard" $ "hazard"
                " (web API)"
              H.li $ do
                H.a ! href "https://github.com/jml/haverer" $ "haverer"
                " (underlying library)"


errorMessage :: (MonadIO m, ToJSON a) => Status -> a -> ActionT m ()
errorMessage code message = do
  setStatus code
  json (Data.Aeson.object ["message" .= message])


badRequest :: (ToJSON a, MonadIO m) => a -> ActionT m ()
badRequest = errorMessage badRequest400


authenticationRequired :: MonadIO m => ActionT m ()
authenticationRequired = do
  setHeader "WWW-Authenticate" realm
  errorMessage unauthorized401 ("Must log in" :: Text)


-- | An internal error occurred.
--
-- This should be used sparingly. Internal errors ought not be possible.
internalError :: Show a => a -> b
internalError = error . Text.unpack . show
