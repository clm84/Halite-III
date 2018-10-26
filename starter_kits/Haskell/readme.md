# Haskell Starter Kit for Halite III

!! Work In Progress !!


## Halite III
Fun AI programming competition. See [halite.io](http://halite.io).


## Developing your bot in Haskell

First, familiarise yourself with the modules found in src/Hlt:
* the type definitions in Hlt.Types model the game state as set of id and location indexed entities and cells
* the Reader api in Hlt.Api gives you access to updated game data at each turn
* the basic navigation helpers in Hlt.Navigation

Develop your own AI by reimplmenting the function `play :: GameEnv [Command]` located in app/MyBot.hs

You will need [stack](https://docs.haskellstack.org/en/stable/README/) installed and available to compile your bot.


## Missing features / Roadmap

* Allow pre game analysis
* Share state between turns
* Logging
* Random generator
