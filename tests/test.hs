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


import Test.Hspec.Wai
import Test.Tasty
import Test.Tasty.Hspec

import Web.Scotty (scottyApp)

import Hazard (hazardWeb)


spec :: Spec
spec = with (scottyApp hazardWeb) $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200


suite :: IO TestTree
suite = do
  spec' <- testSpec "Specification" spec
  return $ testGroup "Hazard" [spec']


main :: IO ()
main = do
  suite' <- suite
  defaultMain suite'
