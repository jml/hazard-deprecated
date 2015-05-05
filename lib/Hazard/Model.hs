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

module Hazard.Model ( GameCreationRequest(..)
                    , Seconds
                    , createGame
                    , reqTurnTimeout
                    , turnTimeout
                    ) where

import qualified Haverer.Game as H


type Seconds = Int

data GameCreationRequest = GameCreationRequest {
  reqNumPlayers :: Int,
  reqTurnTimeout :: Seconds
  } deriving (Eq, Show)


data GameSlot a = GameSlot { turnTimeout :: Seconds,
                             creator :: a,
                             gameState :: Game a }

data Game a = Pending { _numPlayers :: Int
                      , _players :: [a]
                      }
            | InProgress { game :: H.Game a }


createGame :: a -> GameCreationRequest -> GameSlot a
createGame userId request = GameSlot { turnTimeout = reqTurnTimeout request }
