# werewolf

[![Project Status: Wip - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/1.0.0/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/hjwylde/werewolf.svg?branch=master)](https://travis-ci.org/hjwylde/werewolf)
[![Release](https://img.shields.io/github/release/hjwylde/werewolf.svg)](https://github.com/hjwylde/werewolf/releases/latest)

A game engine for running werewolf in a chat client.
This engine is based off of [Werewolves of Millers Hollow](http://www.games-wiki.org/wiki/Werewolves_of_Millers_Hollow/).

### Game description

Deep in the American countryside, the little town of Millers Hollow has recently been infiltrated by werewolves.
Each night, murders are committed by the villagers, who due to some mysterious phenomenon (possibly the greenhouse effect) have become werewolves.
It is now time to take control and eliminate this ancient evil, before the town loses its last few inhabitants.

Objective of the Game:  
For the villagers: kill all of the werewolves.  
For the werewolves: kill all of the villagers.

#### Roles

The current implemented roles are:
* Seer.
* Villager.
* Werewolf.

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
> werewolf --caller @foo start @foo @bar @baz @qux @quux @corge @grault
{"ok":true,"messages":[{"to":null,"message":"Night falls, the town is asleep. The werewolves wake
up, recognise one another and choose a new victim."},{"to":["@foo"],"message":"You slip away
silently from your home."},{"to":["@bar"],"message":"ZzZZzz, you're sound asleep."},...]}
```

In this example, user _@foo_ ran the `start` command with the player names as arguments.
Note that the calling user, _@foo_ was passed in to the `--caller` option.
All commands require this option.

Any command ran returns a JSON result.
The result contains a boolean for whether the command was successful and a list of messages.
The `to` header on a message may either be `null` for a public message or have a list of intended
    recipients.

Let's have _@foo_, a werewolf, vote to kill a villager.
```bash
> werewolf --caller @foo vote @bar
{"ok":true,"messages":[]}
```

This time, even though the command was successful, there are no messages.
In this implementation of werewolf votes are only revealed once tallied.

```bash
> werewolf --caller @foo vote @bar
{"ok":false,"messages":[{"to":["@foo"],"message":"You've already voted!"}]}
```

Here the command was unsuccessful and an error message is sent to _@foo_.
Note that even though the command was unsuccessful, the chat client interface probably won't need to
    do anything special.
Relaying the error message back to the user should suffice.

```bash
> werewolf --caller @qux vote @bar
{"ok":true,"messages":[{"to":["@foo","@baz"],"message":"@baz voted to kill
@bar."},{"to":["@foo","@baz"],"message":"@foo voted to kill @bar."},{"to":null,"message":"The sun
rises. Everybody wakes up and opens their eyes..."},{"to":null,"message":"As you open them you
notice a door broken down and @bar's guts spilling out over the cobblestones. From the look of their
personal effects, you deduce they were a Villager."}]}
```

And so on.

Thus a chat client interface must implement the following:
* The ability to call werewolf commands. This includes passing the `--caller` option and arguments
  correctly. Note that it is possible to just implement the `interpret` command, which interprets
  the caller's input.
* The ability to send resultant messages. Resultant messages may be to everyone or to specific
  users.

#### Commands

See `werewolf --help`.

#### Chat clients

**Coming soon:**
* Slack
