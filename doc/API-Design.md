# API Design

Initial draft.

Focuses on core game playing API, rather than ancillary APIs for user
registration and the like.

Assumes an existing authentication system from which it's possible to derive
the user id of the actor (i.e. the logged-in user).

All requests & responses are JSON unless otherwise specified.


## Kick off a game

### Request

```
POST /games HTTP/1.1

{
  numPlayers: <Int>,
  turnTimeout: <Seconds>
}
```

### Response

```
HTTP/1.1 201 Created
Location: /game/<id>

{
  numPlayers: <Int>,
  turnTimeout: <Seconds>,
  state: "pending",
  creator: <UserId>,
  players: [<UserId>]
}
```

### Errors

* 400 Invalid number of players
* 400 Too long a time limit
* 400 Non-positive time limit


## Any games to play?

### Request

```GET /games?open HTTP/1.1```

### Response

```
HTTP/1.1 200 OK

/game/<id1>
/game/<id2>
```

### Errors

* 400 Unknown status

## Join a game

### Request

`POST /game/<id> HTTP/1.1`

Empty body.

### Response

```
HTTP/1.1 200 OK

{
  numPlayers: <Int>,
  turnTimeout: <Seconds>,
  state: "pending",
  creator: <UserId>,
  players: [<UserId>, <UserId>]
}
```

### Errors

* 4?? Game is already started (i.e. now full)

## What's in a game

`GET /game/<id> HTTP/1.1`

* Are we still waiting for people?
* Who's playing?
* What are the scores?
* Who won?
* Where's the current round?
* Date created
* Who created it

## Changing the game

Before the game has started, you can PATCH the time limit, and also the player
set.

## What's in a round?

`GET /game/<id>/round/<id> HTTP/1.1`

* Whose turn is it?
* What are everyone's discards?
* Who is still in the round? / What is the play order?
* Who is protected by the Priestess?
* How many cards are left?
* Is it over? Who won? Who survived? What was the burn card?

## Is it my turn?

`GET /game/<id>/round/<id>/player/<id> HTTP/1.1`

If that's you:
* hand

otherwise:
* discards
* active
* protected

Note this method might show you that you've busted out. You'll still need to
`POST` to the round for play to proceed.

### Errors

* 404 No such player

## It's my turn!

`POST /game/<id>/round/<id> HTTP/1.1`

`Play Card`

```
HTTP/1.1 201 Created
/game/<id>/round/<id>/turn/<id>
```

### Errors

* 400 Invalid combination
* 403 Not your turn
* 403 Inactive player
* 404 No such player
* 405 Round over
* 408 Request Timeout

## What happened?

`GET /game/<id>/round/<id>/turns HTTP/1.1`

```
HTTP/1.1 200 Created
[Result]
```

## Who am I

`GET /me HTTP/1.1`

```
303 /users/<id>
```

# Notes

Because we can't rely on anyone doing anything in time, we'll need timeouts.
The simplest way to set this is a maximum time between your turn starting and
you deciding what to do with it.

A possibly nicer way is to have two timers, one timer that lasts until you
have polled to discover that it is indeed your turn, and then a second timer
starting from there until you play your turn.
