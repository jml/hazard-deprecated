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
  , game
  , users
    -- | Errors
  , authenticationRequired
  , badRequest
  , errorMessage
  , internalError
  ) where

import BasicPrelude

import Data.Aeson (ToJSON, (.=), encode, object)
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (Foldable, toList)
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
  renderRoute,
  setStatus,
  setHeader
  )

import Hazard.Users (UserID)
import Hazard.Games (GameSlot)
import qualified Hazard.Routes as Route


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


-- | A link where the text is the URL.
--
-- e.g.
--  selfLink "/users" === <a href="/users">/users</a>
selfLink :: Text -> H.Html
selfLink url = simpleLink url url


-- | A link where the URL and the text are both simple text, without markup.
simpleLink :: Text -> Text -> H.Html
simpleLink url text = H.a ! href (H.textValue url) $ H.text text


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
        H.ul $ do
          H.li $ do
            selfLink (renderRoute Route.games)
            H.text " – browse and create Hazard games"
          H.li $ do
            selfLink (renderRoute Route.users)
            H.text " – register an account"
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
          "{\"numPlayers\":3,\"turnTimeout\":3600}"
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
        H.ul $ forM_ gamesLinks (H.li . selfLink)
    gamesLinks = [renderRoute Route.game i | i <- [0..length games' - 1]]


game :: MonadIO m => Int -> GameSlot -> View m
game i g =
  dualResponse g $
  H.docTypeHtml $ do
    H.head $ H.title $ do
      "Hazard :: Game "
      H.text $ show i
    H.body $ do
      H.h1 $ do
        "/game/"
        H.text $ show i
      H.p "Endpoint for a game of Hazard."
      H.h2 "GET"
      H.p "Returns the current state of the game."
      jsonExample g
      H.h2 "POST"
      H.p "Join the game as the logged in user"
      H.p "e.g."
      H.pre $ H.text $ unlines [
        "POST /games",
        ""
        ]
      H.p $ do
        "If successful, will respond with "
        H.code "200"
        " and a representation of the game, indicating that the player has joined."
      H.p $ do
        "If the player has already joined, then will also respond with "
        H.code "200"
        " and the game's representation."
      H.p $ do
        "Will fail with a "
        H.code "400"
        " if the game has already started, or already finished."
      H.p "e.g"
      H.pre $ H.text "{\"message\":\"Game already started\"}"


jsonExample :: ToJSON a => a -> H.Html
jsonExample = H.pre . H.text . decodeUtf8 . toStrict . encode


users :: (Functor f, Foldable f, MonadIO m) => f (UserID, Text) -> View m
users users' =
  dualResponse j h
  where
    j = toList (map snd users')
    h = H.docTypeHtml $ do
      H.head $ H.title "Hazard :: Users"
      H.body $ do
        H.h1 "/users"
        H.p "The user system for Hazard is very simple, and is subject to change. "
        H.p $ do
          "Users can be created by POSTing to this endpoint. This will create a "
          "user with the requested name and assign a random password."
        H.p "Passwords cannot be changed or recovered."
        H.p $ do
          "All authentication and user creation is done "
          H.b "in cleartext"
          " using basic HTTP authentication."
        H.h2 "GET"
        H.p "Returns a list of users."
        H.h2 "POST"
        H.p $ do
          "Will create a user, assign a random password and return a link to "
          "the user in the "
          H.code "Location"
          " header, and the password in the JSON response."
        H.p "e.g."
        H.pre $ H.text $ unlines [
          "POST /users",
          "",
          "{\"username\":\"boris\" }"
          ]
        H.p $ do
          "will create a user, "
          H.code "boris"
          ", and return the following response: "
        H.pre $ H.text $ unlines [
          "HTTP/1.1 201 Created",
          "Content-Type: text/json",
          "",
          "{\"password\":\"A4LVT4NWTY7UHUSY\" }"
          ]
        H.p $ do
          "If the user already exists, will return "
          H.code "400"
          " and a JSON message saying so."
        H.h2 "All users"
        H.ul $ forM_ users' linkToUser
    linkToUser (userId, username) = simpleLink (renderRoute Route.user userId) username


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
