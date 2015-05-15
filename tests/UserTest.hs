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
{-# LANGUAGE QuasiQuotes #-}


module UserTest (suite) where


import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.Tasty
import Test.Tasty.Hspec

import Utils (hazardTestApp, getAs)


suite :: IO TestTree
suite = testSpec "User API" spec

spec :: Spec
spec = with hazardTestApp $

  describe "/users" $ do

    it "GET responds with 200 and an empty list" $
      get "/users" `shouldRespondWith` [json|[]|] {matchStatus = 200}

    it "POST of valid new user responds with 201 and password" $
      post "/users" [json|{username: "foo"}|] `shouldRespondWith`
        [json|{password: "password"}|] {matchStatus = 201, matchHeaders = ["Location" <:> "/user/0"]}

    it "GET includes created users" $ do
      post "/users" [json|{username: "foo"}|]
      get "/users" `shouldRespondWith` [json|["foo"]|] {matchStatus = 200}

    it "POSTs of two new users responds with 201s and passwords" $ do
      post "/users" [json|{username: "foo"}|] `shouldRespondWith`
        [json|{password: "password"}|] {matchStatus = 201, matchHeaders = ["Location" <:> "/user/0"]}
      post "/users" [json|{username: "bar"}|] `shouldRespondWith`
        [json|{password: "password"}|] {matchStatus = 201, matchHeaders = ["Location" <:> "/user/1"]}

    it "POST will not create duplicate users" $ do
      post "/users" [json|{username: "foo"}|]
      post "/users" [json|{username: "foo"}|] `shouldRespondWith`
        [json|{message: "username already exists"}|] {matchStatus = 400}

    it "If not logged in, cannot GET user" $ do
      post "/users" [json|{username: "foo"}|]
      get "/user/0" `shouldRespondWith` requiresAuth

    it "If not logged in, must authenticate even before getting non-existent users" $
      get "/user/0" `shouldRespondWith` requiresAuth

    it "Can GET user after creating" $ do
      post "/users" [json|{username: "foo"}|]
      getAs "foo" "/user/0" `shouldRespondWith` [json|{username: "foo"}|] {matchStatus = 200}

    it "Can't GET users who don't exist" $ do
      post "/users" [json|{username: "foo"}|]
      getAs "foo" "/user/1" `shouldRespondWith` [json|{message: "no such user"}|] {matchStatus = 404}

    it "Creates users with sequential IDs" $ do
      post "/users" [json|{username: "foo"}|]
      post "/users" [json|{username: "bar"}|] `shouldRespondWith`
        [json|{password: "password"}|] {matchStatus = 201, matchHeaders = ["Location" <:> "/user/1"]}
      getAs "foo" "/user/0" `shouldRespondWith` [json|{username: "foo"}|] {matchStatus = 200}
      getAs "foo" "/user/1" `shouldRespondWith` [json|{username: "bar"}|] {matchStatus = 200}


    where requiresAuth = "Basic authentication is required" { matchStatus = 401
                                                            , matchHeaders = [
                                                              "WWW-Authenticate" <:> "Basic realm=\"Hazard API\""
                                                              ]
                                                            }
