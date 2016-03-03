# werewolf

[![Project Status: Wip - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/1.0.0/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/hjwylde/werewolf.svg?branch=master)](https://travis-ci.org/hjwylde/werewolf)
[![Release](https://img.shields.io/github/release/hjwylde/werewolf.svg)](https://github.com/hjwylde/werewolf/releases/latest)

A game engine for running werewolf in a chat client.
This engine is based off of [Werewolves of Millers Hollow](http://www.games-wiki.org/wiki/Werewolves_of_Millers_Hollow/).

### Game description

Deep in the American countryside, the little town of Millers Hollow has recently been infiltrated by Werewolves.
Each night, murders are committed by the Villagers, who due to some mysterious phenomenon (possibly the greenhouse effect) have become Werewolves.
It is now time to take control and eliminate this ancient evil, before the town loses its last few inhabitants.

Objective of the Game:  
For the Angel: die in the first round.  
For the Villagers: lynch all of the Werewolves.  
For the Werewolves: devour all of the Villagers.

#### Roles

The implemented roles are split into four categories.

**The Ambiguous:**

The Ambiguous may change allegiance during the game.

* Wild-child.
* Wolf-hound.

**The Loners:**

The Loners have their own win condition.

* Angel.

**The Villagers:**

The Villagers must lynch all of the Werewolves.

* Bear Tamer.
* Defender.
* Scapegoat.
* Seer.
* Simple Villager.
* Village Idiot.
* Villager-Villager.
* Witch.

**The Werewolves:**

The Werewolves must devour all of the Villagers.

* Simple Werewolf.

### Installing

Installing werewolf is easiest done using either
    [stack](https://github.com/commercialhaskell/stack) (recommended) or
    [Cabal](https://github.com/haskell/cabal).

**Using stack:**

```bash
stack install werewolf
export PATH=$PATH:~/.local/bin
```

**Using Cabal:**

```bash
cabal-install werewolf
export PATH=$PATH:~/.cabal/bin
```

### Usage

This section covers how a chat client interacts with the werewolf game engine.

All werewolf commands are designed to be run by a user from the chat client.
E.g., to start a game:

```bash
> werewolf --caller @foo start --extra-roles seer @bar @baz @qux @quux @corge @grault
{"ok":true,"messages":[
    {"to":null,"message":"A new game of werewolf is starting with @foo, @bar, @baz, @qux, @quux, @corge, @grault!"},
    {"to":null,"message":"The roles in play are Seer (1), Simple Villager (4), Simple Werewolf (2) for a total balance of -2."},
    {"to":"@foo","message":"You're a Simple Villager.\nA simple, ordinary townsperson in every way. Their only weapons are the ability to analyze behaviour to identify Werewolves, and the strength of their conviction to prevent the execution of the innocents like themselves."},
    ...,
    {"to":null,"message":"Night falls, the village is asleep."},
    {"to":null,"message":"The Seer wakes up."},
    {"to":"@corge","message":"Whose allegiance would you like to `see`?"}
    ]}
```

In this example, user _@foo_ ran the `start` command with the player names as arguments.
Note that the calling user, _@foo_ was passed in to the `--caller` option.
All commands require this option.

Any command ran returns a JSON result.
The result contains a boolean for whether the command was successful and a list of messages.
The `to` header on a message may either be `null`---for a public message---or have an intended
    recipient.

It's the Seer's turn now.

```bash
> werewolf --caller @corge see @grault
{"ok":true,"messages":[
    {"to":"@corge","message":"@grault is aligned with the Werewolves."},
    {"to":"@quux","message":"You feel restless, like an old curse is keeping you from sleep. It seems you're not the only one... @grault are also emerging from their homes."},
    {"to":"@grault","message":"You feel restless, like an old curse is keeping you from sleep.  It seems you're not the only one... @quux are also emerging from their homes."},
    {"to":null,"message":"The Werewolves wake up, recognise one another and choose a new victim."},
    {"to":"@quux","message":"Whom would you like to `vote` to devour?"},
    {"to":"@grault","message":"Whom would you like to `vote` to devour?"}
    ]}
```

Let's have the Werewolves, _@quux_ and _@grault_, vote to devour a Villager.

```bash
> werewolf --caller @quux vote @foo
{"ok":true,"messages":[
    {"to":"@grault","message":"@quux voted to devour @foo."}
    ]}
> werewolf --caller @grault vote @foo
{"ok":true,"messages":[
    {"to":"@quux","message":"@grault voted to devour @foo."},
    {"to":null,"message":"The sun rises. Everybody wakes up and opens their eyes..."},
    {"to":null,"message":"As you open them you notice a door broken down and @foo's guts half devoured and spilling out over the cobblestones. From the look of their personal effects, you deduce they were a Simple Villager."},
    {"to":null,"message":"As the village gathers in the square the town clerk calls for a vote."},
    {"to":null,"message":"Whom would you like to `vote` to lynch?"}
    ]}
```

Too bad for _@foo_. Maybe the village can get some vengeance...

```bash
> werewolf --caller @corge vote @grault
{"ok":true,"messages":[]}
```

This time, even though the command was successful, there are no messages.

```bash
> werewolf --caller @corge vote @grault
{"ok":false,"messages":[{"to":"@corge","message":"You've already voted!"}]}
```

Here the command was unsuccessful and an error message is sent to _@corge_.
Even though the command was unsuccessful, the chat client interface probably won't need to do
    anything special.
Relaying the error message back to the user should suffice.

Thus a chat client interface must implement the following:
* The ability to call werewolf commands. This includes passing the `--caller` option and arguments
  correctly. It is possible to only implement the `interpret` command, which interprets the
  caller's input.
* The ability to send resultant messages. Resultant messages may be to everyone or to a specific
  user.

#### Commands

See `werewolf --help`.

#### Chat clients

**Coming soon:**
* Slack
