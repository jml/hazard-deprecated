#!/bin/bash
#
# A sketch of how you might talk to the server. Not actually a functional
# client, as it doesn't read responses.

# Accept and post JSON
CURL='curl -H "Accept: application/json" -H "Content-type: application/json'

# The server to talk to
HOST=http://localhost:3000

## Create a user 'foo'. Password is in response.
$CURL -d  '{"username":"foo"}' $HOST/users

# e.g. {"password":"11DLI80SQCHGDNK4"}


## Fetch a list of all registered users.
$CURL -u foo:11DLI80SQCHGDNK4 $HOST/users

# e.g. ["foo"]


## Register a game
$CURL -u foo:11DLI80SQCHGDNK4 -d '{"turnTimeout":3600,"numPlayers":2}' $HOST/games

# e.g. {"creator":"0","state":"pending","players":{"0":null},"turnTimeout":3600,"numPlayers":2}


## Create a user 'bar'.
$CURL -d '{"username":"bar"}' $HOST/users

# e.g. {"password":"GC8EGGKGKL8MCA56"}


## 'bar' joins the game.
$CURL -u bar:GC8EGGKGKL8MCA56 -d '' $HOST/game/0

# e.g. {"creator":"0","state":"in-progress","players":{"0":0,"1":0},"turnTimeout":3600,"numPlayers":2}


## foo's view of the game.
$CURL -u foo:11DLI80SQCHGDNK4 $HOST/game/0

# e.g. {"creator":"0","state":"in-progress","players":{"0":0,"1":0},"turnTimeout":3600,"numPlayers":2}


## foo's view of the round
$CURL -u foo:11DLI80SQCHGDNK4 $HOST/game/0/round/0

# e.g. {"dealtCard":"Priestess","currentPlayer":"0","players":[{"protected":false,"active":true,"id":"0","hand":"Knight","discards":[]},{"protected":false,"active":true,"id":"1","discards":[]}]}


## foo plays the priestess
$CURL -u foo:11DLI80SQCHGDNK4 -d '{"card":"priestess"}' $HOST/game/0/round/0

# e.g. {"card":"Priestess","protected":"0","result":"protected","id":"0"}


## bar's view of the round after foo's play
$CURL -u bar:GC8EGGKGKL8MCA56 $HOST/game/0/round/0

# e.g. {"dealtCard":"Soldier","currentPlayer":"1","players":[{"protected":true,"active":true,"id":"0","discards":["Priestess"]},{"protected":false,"active":true,"id":"1","hand":"Soldier","discards":[]}]}


## bar plays a soldier, guessing Prince
$CURL -u bar:GC8EGGKGKL8MCA56 -d '{"card":"soldier","guess":"prince","target":"0"}' $HOST/game/0/round/0

# e.g. {"card":"Soldier","guess":"Prince","result":"no-change","id":"1","target":"0"}


## foo checks their hand
$CURL -u foo:11DLI80SQCHGDNK4 $HOST/game/0/round/0

# e.g. {"dealtCard":"Knight","currentPlayer":"0","players":[{"protected":false,"active":true,"id":"0","hand":"Knight","discards":["Priestess"]},{"protected":false,"active":true,"id":"1","discards":["Soldier"]}]}


## foo plays a Knight
$CURL -u foo:11DLI80SQCHGDNK4 -d '{"card":"knight","target":"1"}' $HOST/game/0/round/0

# e.g. {"card":"Knight","result":"eliminated","id":"0","eliminated":"1","target":"1"}


## foo checks the current state of the first round
$CURL -u foo:11DLI80SQCHGDNK4  $HOST/game/0/round/0

# e.g. {"currentPlayer":null,"winners":["0"],"players":[{"protected":false,"active":true,"id":"0","hand":"Knight","discards":["Knight","Priestess"]},{"protected":null,"active":false,"id":"1","discards":["Soldier","Soldier"]}]}


## foo looks at their hand for the next round
$CURL -u foo:11DLI80SQCHGDNK4  $HOST/game/0/round/1

# e.g. {"dealtCard":"Knight","currentPlayer":"0","players":[{"protected":false,"active":true,"id":"0","hand":"Clown","discards":[]},{"protected":false,"active":true,"id":"1","discards":[]}]}
