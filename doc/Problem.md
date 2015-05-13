# Problem statement

We want to play Love Letter online.

## Goals

We want to support the following:

1. Mostly trusted people write bots that play loveletter
2. Untrusted people write bots that play loveletter
3. A pretty website for playing loveletter interactively

These are in order of importance.

## Constraints

* Relatively easy to write a client in Haskell, Python, and Clojure
* Cannot rely on clients being coordinated through any means other than the
  server

## Assumptions

* Level of traffic is going to be vanishingly small, so scaling considerations
  can wait
* Initial users will be particularly patient and understanding, and so we do
  not need explicit versioning yet

## Technology

* RESTful API using HTTP 1.1.
* JSON for resource serialization.
* Ideally OAuth2 for authentication, but initially HTTP basic
