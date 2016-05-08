# werewolf

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/hjwylde/werewolf.svg?branch=master)](https://travis-ci.org/hjwylde/werewolf)
[![Release](https://img.shields.io/github/release/hjwylde/werewolf.svg)](https://github.com/hjwylde/werewolf/releases/latest)
[![werewolf on Stackage LTS](https://www.stackage.org/package/werewolf/badge/lts)](https://www.stackage.org/lts/package/werewolf)
[![werewolf on Stackage Nightly](https://www.stackage.org/package/werewolf/badge/nightly)](https://www.stackage.org/nightly/package/werewolf)

A game engine for playing werewolf within an arbitrary chat client.
Werewolf is a well known social party game, commonly also called Mafia.
See the [Wikipedia article](https://en.wikipedia.org/wiki/Mafia_(party_game)) for a rundown on its
    gameplay and history.

If you're here just to play werewolf, you may wish to skip straight to
    [chat interfaces](https://github.com/hjwylde/werewolf#chat-interfaces).

### Game description

Long has the woods been home to wild creatures, both kind and cruel.
Most have faces and are known to the inhabitants of Fougères in Brittany, France; but no-one from
    the village has yet to lay eyes on the merciless Werewolf.

Each night Werewolves attack the village and devour the innocent.
For centuries no-one knew how to fight this scourge, however recently a theory has taken ahold that
    mayhaps the Werewolves walk among the Villagers themselves...

Objective of the game:  
For the Loners: complete their own objective.  
For the Villagers: lynch all of the Werewolves.  
For the Werewolves: devour all of the Villagers.

#### Roles

The implemented roles are split into four categories.

**The Ambiguous:**

No-one knows the true nature of the Ambiguous, sometimes not even the Ambiguous themselves!

The Ambiguous are able to change allegiance throughout the game.

* Orphan
* Village Drunk

**The Loners:**

The Loners look out for themselves and themselves alone.

The Loners must complete their own objective.

* Fallen Angel
* Spiteful Ghost

**The Villagers:**

Fraught with fear of the unseen enemy, the Villagers must work together to determine the truth and
    eliminate the threat to Fougères.
The task before them will not be easy, but a certain few have learnt some tricks over the years that
    may turn out rather useful.

The Villagers must lynch all of the Werewolves.

* Beholder
* Crooked Senator
* Druid
* Hunter
* Jester
* Lycan
* Medusa
* Oracle
* Protector
* Scapegoat
* Seer
* Simple Villager
* True Villager
* Witch

**The Werewolves:**

Hiding in plain sight, the Werewolves are not a small trifle.

The Werewolves must devour all of the Villagers.

* Alpha Wolf
* Simple Werewolf

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

This section covers how a chat interface interacts with the werewolf game engine.

All werewolf commands are designed to be run by a user from the chat client.
E.g., to start a game:

```bash
> werewolf --caller @foo --tag werewolf start --extra-roles seer @bar @baz @qux @quux @corge @grault
{"ok":true,"messages":[
    {"to":null,"message":"A new game of werewolf is starting with @foo, @bar, @baz, @qux, @quux, @corge and @grault!"},
    {"to":null,"message":"The roles in play are Seer (1), Simple Villager (4) and Simple Werewolf (2) for a total balance of -2."},
    {"to":"@foo","message":"You're a Simple Villager.\nA simple, ordinary townsperson in every way. Some may be cobblers, others bakers or even nobles. No matter their differences though, the plight of Werewolves in Fougères unites all the Villagers in this unfortunate time.\nThe Simple Villager has no special abilities, they must use their guile to determine whom among them is not who they say they are."},
    ...,
    {"to":null,"message":"Night falls, the village is asleep."},
    {"to":null,"message":"The Seer wakes up."},
    {"to":"@corge","message":"Whose allegiance would you like to `see`?"}
    ]}
```

In this example, user _@foo_ ran the `start` command with the player names as arguments.
Note that the calling user, _@foo_, was passed in to the `--caller` option and a game tag,
    _werewolf_, was passed in to the `--tag` option.
All commands require these options (n.b., the tag option is arbitrary, it just enables multiple
    games of werewolf to be running at once).

Any command ran returns a JSON result.
The result contains a boolean for whether the command was successful and a list of messages.
The `to` header on a message may either be `null`---for a public message---or have an intended
    recipient.

It's the Seer's turn now.

```bash
> werewolf --caller @corge --tag werewolf see @grault
{"ok":true,"messages":[
    {"to":"@corge","message":"@grault is aligned with the Werewolves."},
    {"to":"@quux","message":"You feel restless, like an old curse is keeping you from sleep. It seems you're not the only one... @grault is also emerging from their home."},
    {"to":"@grault","message":"You feel restless, like an old curse is keeping you from sleep. It seems you're not the only one... @quux is also emerging from their home."},
    {"to":null,"message":"The Werewolves wake up, transform and choose a new victim."},
    {"to":"@quux","message":"Whom would you like to `vote` to devour?"},
    {"to":"@grault","message":"Whom would you like to `vote` to devour?"}
    ]}
```

Let's have the Werewolves, _@quux_ and _@grault_, vote to devour a Villager.

```bash
> werewolf --caller @quux --tag werewolf vote @foo
{"ok":true,"messages":[
    {"to":"@grault","message":"@quux voted to devour @foo."}
    ]}
> werewolf --caller @grault --tag werewolf vote @foo
{"ok":true,"messages":[
    {"to":"@quux","message":"@grault voted to devour @foo."},
    {"to":null,"message":"The sun rises. Everybody wakes up and opens their eyes..."},
    {"to":null,"message":"As you open them you notice a door broken down and @foo's guts half devoured and spilling out over the cobblestones. From the look of their personal effects, you deduce they were a Simple Villager."},
    {"to":null,"message":"As the village gathers in the square the Town Clerk calls for a vote."},
    {"to":null,"message":"Whom would you like to `vote` to lynch?"}
    ]}
```

Too bad for _@foo_. Maybe the village can get some vengeance...

```bash
> werewolf --caller @corge --tag werewolf vote @grault
{"ok":true,"messages":[]}
```

This time, even though the command was successful, there are no messages.

```bash
> werewolf --caller @corge --tag werewolf vote @grault
{"ok":false,"messages":[{"to":"@corge","message":"You've already voted!"}]}
```

Here the command was unsuccessful and an error message is sent to _@corge_.
Even though the command was unsuccessful, the chat interface probably won't need to do anything
    special.
Relaying the error message back to the user should suffice.

Thus a chat interface must implement the following:
* The ability to call werewolf commands. This includes passing the `--caller` and `--tag` options
  and arguments correctly. It is possible to only implement the `interpret` command, which
  interprets the caller's input.
* The ability to send resultant messages. Resultant messages may be to everyone or to a specific
  user.

#### Commands

See `werewolf --help`.

#### Chat interfaces

Click through for instructions on how to run a chat interface and play werewolf.

* [Slack](https://github.com/hjwylde/werewolf-slack)
