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

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hazard.Views (
  realm
    -- | Pages
  , home
  , games
    -- | Errors
  , authenticationRequired
  , badRequest
  , errorMessage
  , internalError
  ) where

import BasicPrelude

import Data.Aeson (ToJSON, (.=), object)
import Data.Foldable (Foldable)
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
   _ -> do
     setHeader "Content-Type" "text/html; charset=utf-8"
     lazyBytes . renderHtml $ h

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
        H.p $ do
          "This is currently intended as an API server, rather than an "
          "interactive user interface."
        H.h2 "Endpoints"
        H.ul $
          H.li $ do
            H.a ! href "/games" $ "/games"
            H.text " – browse and create Hazard games"
        H.p "The API will change."
        H.h2 "Source code"
        H.ul $ do
              H.li $ do
                H.a ! href "https://github.com/jml/hazard" $ "hazard"
                " (web API)"
              H.li $ do
                H.a ! href "https://github.com/jml/haverer" $ "haverer"
                " (underlying library)"


games :: (Foldable f, MonadIO m) => f a -> View m
games games' =
  dualResponse j h
  where
    j = gamesLinks
    h = H.docTypeHtml $ do
      H.head $ H.title "Hazard :: Games"
      H.body $ do
        H.h1 "/games"
        H.p "Endpoint for registering and creating games."
        H.h2 "GET"
        H.p "Returns a list of games."
        H.h2 "POST"
        H.p "Will create a game, and return a link to it."
        H.p "e.g."
        H.pre $ H.text $ unlines [
          "POST /games",
          "",
          "{ numPlayers = 3, ",
          "  turnTimeout = 3600 }"
          ]
        H.p $ do
          "will register a game for three players with a turn timeout "
          "of one hour, and return a "
          H.code "201 Created"
          " response with a "
          H.code "Location"
          " header pointing at the newly created game."
        H.p $ do
          "The creator of the game is the logged-in user, who is also "
          "automatically added to the game."
        H.h2 "All games, past and present"
        H.ul $ forM_ gamesLinks selfLink
    gamesLinks = ["/game/" ++ show i | i <- [0..length games' - 1]]
    selfLink url = H.a ! href (H.textValue url) $ H.text url

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
