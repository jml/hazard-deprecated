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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ModelTest (suite) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Hazard.Model (GameCreationRequest(..),
                     createGame,
                     numPlayers,
                     turnTimeout,
                     reqTurnTimeout)

instance Arbitrary GameCreationRequest where
  arbitrary = GameCreationRequest <$> arbitrary <*> arbitrary


suite :: TestTree
suite = testGroup "Hazard.Model" [
  testGroup "QuickCheck tests"
  [ testProperty "createGame has turnTimeout" $
    \x -> \y -> turnTimeout (createGame (x :: Int) y) == reqTurnTimeout y
  , testProperty "createGame has numPlayers" $
    \x -> \y -> numPlayers (createGame (x :: Int) y) == reqNumPlayers y
    ]
  ]