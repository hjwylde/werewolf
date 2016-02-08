## Changelog

#### Upcoming

*Revisions*

* Privatised underscore methods and changed old uses to using lens. ([#20](https://github.com/hjwylde/werewolf/issues/20))

#### v0.4.3.1

*Revisions*

* Added missing module to Cabal file.

#### v0.4.3.0

*Minor*

* Added the Defender role. ([#38](https://github.com/hjwylde/werewolf/issues/38))

#### v0.4.2.2

*Revisions*

* Added missing module to Cabal file.

#### v0.4.2.1

*Revisions*

* Fixed a bug causing the Witch being unable to heal themselves. ([#76](https://github.com/hjwylde/werewolf/issues/76))

#### v0.4.2.0

*Minor*

* Added the Villager-Villager role. ([#37](https://github.com/hjwylde/werewolf/issues/37))

#### v0.4.1.2

*Revisions*

* Added missing module to Cabal file.

#### v0.4.1.1

*Revisions*

* Fixed a bug causing the Witch being unable to heal themselves. ([#76](https://github.com/hjwylde/werewolf/issues/76))

#### v0.4.1.0

*Minor*

* Added the Witch role. ([#5](https://github.com/hjwylde/werewolf/issues/5))

#### v0.4.0.0

*Major*

* Restricted count of special roles to 1. ([#32](https://github.com/hjwylde/werewolf/issues/32))
* Changed private message structure to only ever be for a single player. ([#21](https://github.com/hjwylde/werewolf/issues/21))

*Minor*

* Renamed `turn` to `stage`. ([#70](https://github.com/hjwylde/werewolf/issues/70))
* Renamed `VillagersTurn` to `VillagesTurn`. ([#70](https://github.com/hjwylde/werewolf/issues/70))
* Added `events` to the game state. ([#71](https://github.com/hjwylde/werewolf/issues/71))
* Added private pinging to the `ping` command. ([#69](https://github.com/hjwylde/werewolf/issues/69))
* Restricted `end` to players in the current game. ([#74](https://github.com/hjwylde/werewolf/issues/74))

#### v0.3.4.0

*Minor*

* Added a `ping` command. ([#64](https://github.com/hjwylde/werewolf/issues/64))

*Revisions*

* Added missing apostrophe to the new turn message. ([#63](https://github.com/hjwylde/werewolf/issues/63))
* Changed the "Whom would you like to lynch?" text to be a public message displayed after the devoured message. ([#56](https://github.com/hjwylde/werewolf/issues/56))
* Better prompt to action when villagers vote.
* Changed devour vote messages to be sent immediately. ([#57](https://github.com/hjwylde/werewolf/issues/57))
* Removed useless `only` function. ([#55](https://github.com/hjwylde/werewolf/issues/55))
* Turned start of day and night into distinct turns.
* Added private message to players when the game is over. ([#65](https://github.com/hjwylde/werewolf/issues/65))

#### v0.3.3.2

*Revisions*

* Fixed missing file in Cabal file. ([#18](https://github.com/hjwylde/werewolf/issues/18))

#### v0.3.3.1

*Revisions*

* Added `noIntersperse` to `interpret`. ([#60](https://github.com/hjwylde/werewolf/issues/60))

#### v0.3.3.0

*Minor*

* Added a Scapegoat role. ([#40](https://github.com/hjwylde/werewolf/issues/40))
* Added a `status` command. ([#18](https://github.com/hjwylde/werewolf/issues/18))

*Revisions*

* Added `--` to help description of `interpret`. ([#60](https://github.com/hjwylde/werewolf/issues/60))

#### v0.3.2.0

*Minor*

* Added a "Whom would you like to lynch?" message during the Villagers' turn. ([#25](https://github.com/hjwylde/werewolf/issues/25))
* Allowed lowercase roles for `--extra-roles` in the `start` command. ([#33](https://github.com/hjwylde/werewolf/issues/33))

*Revisions*

* Shrunk some of the help text to make it more readable. ([#25](https://github.com/hjwylde/werewolf/issues/25))

#### v0.3.1.3

*Revisions*

* Fixed a bug where Werewolves could devour other Werewolves. ([#34](https://github.com/hjwylde/werewolf/issues/34))
* Changed Werewolf text from "kill" to "devour". ([#34](https://github.com/hjwylde/werewolf/issues/34))

#### v0.3.1.2

*Revisions*

* Fixed dead werewolves being informed of votes. ([#24](https://github.com/hjwylde/werewolf/issues/24))

#### v0.3.1.1

*Revisions*

* Tidied up the help text to be smaller. ([#26](https://github.com/hjwylde/werewolf/issues/26))
* Fixed a bug where the turn was advanced to Werewolves when no Werewolves were alive. ([#26](https://github.com/hjwylde/werewolf/issues/26))

#### v0.3.1.0

*Minor*

* Added a message to say the names of all the players at the start of a game. ([#23](https://github.com/hjwylde/werewolf/issues/23))
* Added a message to say the roles in play at the start of a game. ([#16](https://github.com/hjwylde/werewolf/issues/16))

#### v0.3.0.5

*Revisions*

* Fixed a bug where Werewolves could devour other Werewolves. ([#34](https://github.com/hjwylde/werewolf/issues/34))
* Changed Werewolf text from "kill" to "devour". ([#34](https://github.com/hjwylde/werewolf/issues/34))

#### v0.3.0.4

*Revisions*

* Fixed dead werewolves being informed of votes. ([#24](https://github.com/hjwylde/werewolf/issues/24))

#### v0.3.0.3

*Revisions*

* Tidied up the help text to be smaller. ([#26](https://github.com/hjwylde/werewolf/issues/26))
* Fixed a bug where the turn was advanced to Werewolves when no Werewolves were alive. ([#26](https://github.com/hjwylde/werewolf/issues/26))

#### v0.3.0.2

*Revisions*

* Tidied up the help text to be smaller. ([#26](https://github.com/hjwylde/werewolf/issues/26))
* Fixed a bug where the turn was advanced to Werewolves when no Werewolves were alive. ([#26](https://github.com/hjwylde/werewolf/issues/26))

#### v0.3.0.1

*Revisions*

* Fixed `interpret` to display the commands help messages when given invalid arguments. ([#22](https://github.com/hjwylde/werewolf/issues/22))

#### v0.3.0.0

*Major*

* Added `--extra-roles` option to `start`. ([#12](https://github.com/hjwylde/werewolf/issues/12))
* Removed Seer from being included by default. ([#12](https://github.com/hjwylde/werewolf/issues/12))

*Minor*

* Allowed `start` to work when the game has ended but `end` hasn't been called. ([#15](https://github.com/hjwylde/werewolf/issues/15))
* Added `quit` command. ([#13](https://github.com/hjwylde/werewolf/issues/13))

#### v0.2.0.2

*Revisions*

* Fixed dead werewolves being informed of votes. ([#24](https://github.com/hjwylde/werewolf/issues/24))

#### v0.2.0.1

*Revisions*

* Tidied up the help text to be smaller. ([#26](https://github.com/hjwylde/werewolf/issues/26))
* Fixed a bug where the turn was advanced to Werewolves when no Werewolves were alive. ([#26](https://github.com/hjwylde/werewolf/issues/26))

#### v0.2.0.0

*Major*

* Added the Seer role. ([#4](https://github.com/hjwylde/werewolf/issues/4))
* Removed the need to encode / decode to JSON for the state file. ([#9](https://github.com/hjwylde/werewolf/issues/9))

#### v0.1.0.0

*Major*

* Initial implementation with Villagers and Werewolves. ([#1](https://github.com/hjwylde/werewolf/issues/1))
