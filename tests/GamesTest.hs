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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GamesTest (suite) where

import BasicPrelude

import Control.Monad.Random
import Data.Aeson
import Data.Aeson.Types (parseEither)

import Test.Tasty
import Test.Tasty.QuickCheck

import Haverer.Internal.Error
import Haverer.Deck (Card(..))

import Hazard.Games (GameCreationRequest(..),
                     GameSlot,
                     Game(Pending, InProgress),
                     Validated(..),
                     createGame,
                     creator,
                     gameState,
                     joinSlot,
                     numPlayers,
                     players,
                     requestGame,
                     runSlotActionT,
                     turnTimeout,
                     validateCreationRequest)


instance Arbitrary (GameCreationRequest 'Unchecked) where
  arbitrary = requestGame <$> arbitrary <*> arbitrary


instance Arbitrary (GameCreationRequest 'Valid) where
  arbitrary = do
    uncheckedRequest <- requestGame <$> choose (2, 4) <*> (getPositive <$> arbitrary)
    case validateCreationRequest uncheckedRequest of
     Left e -> terror $ show e
     Right r -> return r


instance (Arbitrary a, Ord a, Show a) => Arbitrary (GameSlot a) where

  arbitrary = do
    game <- initialGame
    extraPlayers <- choose (0, numPlayers game - 1)
    addPlayers extraPlayers game


instance Arbitrary Card where

  arbitrary = elements [ Soldier
                       , Clown
                       , Knight
                       , Priestess
                       , Wizard
                       , General
                       , Minister
                       , Prince
                       ]


initialGame :: Arbitrary a => Gen (GameSlot a)
initialGame = createGame <$> arbitrary <*> arbitrary


addPlayers :: (Arbitrary a, Ord a, Show a) => Int -> GameSlot a -> Gen (GameSlot a)
addPlayers n g =
  iterate (>>= addPlayer) (return g) !! n
  where
    addPlayer g' = do
      p <- arbitrary `suchThat` (`notElem` players g')
      return $ snd $ assertRight' (evalRand (runSlotActionT (joinSlot p) g') (mkStdGen 42))


prop_creatorInPlayers :: Eq a => GameSlot a -> Bool
prop_creatorInPlayers g = creator g `elem` players g


prop_numPlayersVsActualPlayers :: GameSlot a -> Bool
prop_numPlayersVsActualPlayers g =
  case gameState g of
   Pending {} -> length (players g) < numPlayers g
   InProgress {} -> length (players g) <= numPlayers g


prop_jsonEquivalent :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
prop_jsonEquivalent value =
  let jsonVersion = toJSON value in
  case parseEither parseJSON jsonVersion of
   Left e -> error e
   Right value' -> value == value'


suite :: TestTree
suite = testGroup "Hazard.Games" [
  testGroup "createGame"
  [ testProperty "uses requested turnTimeout" $
    \x y -> let g = createGame (x :: Int) y in turnTimeout g == reqTurnTimeout y
  , testProperty "uses requested numPlayers" $
    \x y -> let g = createGame (x :: Int) y in numPlayers g == reqNumPlayers y
  , testProperty "records the creator" $
    \x y -> let g = createGame (x :: Int) y in creator g == x
  , testProperty "leaves creator as the only initial player" $
    \x y -> let g = createGame (x :: Int) y in players g == [x]
  ],
  testGroup "GameSlot"
  [ testProperty "creator in players" $
    \x -> prop_creatorInPlayers (x :: GameSlot Int)
  , testProperty "numPlayers greater than or equal to number of players" $
    \x -> prop_numPlayersVsActualPlayers (x :: GameSlot Int)
  ],
  testGroup "JSON parsing"
  [ testProperty "Card" $ \x -> prop_jsonEquivalent (x :: Card)
  ]
  ]
