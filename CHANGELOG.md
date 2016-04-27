# Changelog

### Upcoming

### v1.0.2.1

*Revisions*

* Removed the binary header and updated the program description.

### v1.0.2.0

*Minor*

* Added the Alpha Wolf role. ([#149](https://github.com/hjwylde/werewolf/issues/149))
* Added the Beholder role. ([#168](https://github.com/hjwylde/werewolf/issues/168))
* Added the Lycan role. ([#169](https://github.com/hjwylde/werewolf/issues/169))

*Revisions*

* Moved Seer's player seen message to Sunrise. ([#184](https://github.com/hjwylde/werewolf/issues/184))

### v1.0.1.0

*Minor*

* Added the Crooked Senator role. ([#142](https://github.com/hjwylde/werewolf/issues/142))

### v1.0.0.0

*Major*

* Initial stable release!

### v0.5.4.0

*Minor*

* Added `--include-seer` option to `start`. ([#179](https://github.com/hjwylde/werewolf/issues/179))
* Added the Village Drunk role. ([#160](https://github.com/hjwylde/werewolf/issues/160))

*Revisions*

* Minor updates to the `help rules` text.
* Removed most logical tests and kept error ones. ([#164](https://github.com/hjwylde/werewolf/issues/164))

### v0.5.3.0

*Minor*

* Removed the Devoted Servant. ([#127](https://github.com/hjwylde/werewolf/issues/127))
* Renamed the Angel to the Fallen Angel. ([#130](https://github.com/hjwylde/werewolf/issues/130))
* Renamed the Villager-Villager to the True Villager. ([#137](https://github.com/hjwylde/werewolf/issues/137))
* Removed the Wolf-hound. ([#129](https://github.com/hjwylde/werewolf/issues/129))

*Revisions*

* Changed the Druid's balance to 3. ([#165](https://github.com/hjwylde/werewolf/issues/165))
* Updated the Fallen Angel's description and rules. ([#130](https://github.com/hjwylde/werewolf/issues/130))
* Updated the Simple Werewolf's description. ([#174](https://github.com/hjwylde/werewolf/issues/174))
* Updated the True Villager's description and rules. ([#137](https://github.com/hjwylde/werewolf/issues/137))
* Made the roles gender neutral. ([#101](https://github.com/hjwylde/werewolf/issues/101))
* Made the role descriptions use consistent tenses. ([#158](https://github.com/hjwylde/werewolf/issues/158))

### v0.5.2.0

*Minor*

* Renamed the Wild-child to Orphan. ([#128](https://github.com/hjwylde/werewolf/issues/128))
* Added the Hunter role. ([#3](https://github.com/hjwylde/werewolf/issues/3))

*Revisions*

* Updated the Orphan's description. ([#128](https://github.com/hjwylde/werewolf/issues/128))
* Added role taken message for the Devoted Servant. ([#157](https://github.com/hjwylde/werewolf/issues/157))

### v0.5.1.0

*Minor*

* Renamed the Defender to Protector. ([#132](https://github.com/hjwylde/werewolf/issues/132))
* Automatically delete the game file if the game is over. ([#100](https://github.com/hjwylde/werewolf/issues/100))
* Added errors for using overloaded commands out of turn. ([#100](https://github.com/hjwylde/werewolf/issues/100))

*Revisions*

* Updated the Protector's description and rules. ([#132](https://github.com/hjwylde/werewolf/issues/132))
* Improved the English used. ([#72](https://github.com/hjwylde/werewolf/issues/72))
* Added which player is the Villager-Villager to `status`. ([#144](https://github.com/hjwylde/werewolf/issues/144))

### v0.5.0.0

*Major*

* Added required `--tag` option for enabling multiple games at once. ([#29](https://github.com/hjwylde/werewolf/issues/29))

### v0.4.12.0

*Minor*

* Renamed the Bear Tamer to Druid. ([#131](https://github.com/hjwylde/werewolf/issues/131))
* Renamed the Village Idiot to Jester. ([#136](https://github.com/hjwylde/werewolf/issues/136))

*Revisions*

* Updated the Witch's description and rules. ([#138](https://github.com/hjwylde/werewolf/issues/138))
* Updated the Scapegoat's description. ([#133](https://github.com/hjwylde/werewolf/issues/133))
* Updated the Druid's description and rules. ([#131](https://github.com/hjwylde/werewolf/issues/131))
* Updated the Jester's description and rules. ([#136](https://github.com/hjwylde/werewolf/issues/136))

### v0.4.11.0

*Minor*

* Removed the advice field from Role. ([#134](https://github.com/hjwylde/werewolf/issues/134))

*Revisions*

* Replaced references to Millers Hollow with an original game description. ([#126](https://github.com/hjwylde/werewolf/issues/126))
* Fixed a bug where extra roles with spaces in them weren't recognised.
* Fixed Devoted Servant messages to exclude her when joining the Werewolf pack.
* Fixed Devoted Servant help messages to include how to `pass`.
* Removed player cap of 24. ([#143](https://github.com/hjwylde/werewolf/issues/143))
* Fixed a bug where the Wild Child would receive role model died messages when dead. ([#145](https://github.com/hjwylde/werewolf/issues/145))
* Updated the Simple Villager's description and rules. ([#135](https://github.com/hjwylde/werewolf/issues/135))
* Updated the Seer's description. ([#134](https://github.com/hjwylde/werewolf/issues/134))
* Updated the Simple Werewolf's description. ([#139](https://github.com/hjwylde/werewolf/issues/139))

### v0.4.10.0

*Minor*

* Added `boot` command. ([#14](https://github.com/hjwylde/werewolf/issues/14))

### v0.4.9.0

*Minor*

* Added player contributed messages upon game over. ([#86](https://github.com/hjwylde/werewolf/issues/86))
* Changed `choose` command for Scapegoat to take a space separated list rather than comma separated. ([#98](https://github.com/hjwylde/werewolf/issues/98))
* Filtered `help` commands based on the current game. ([#94](https://github.com/hjwylde/werewolf/issues/94))
* Added `--all` option to `help` commands. ([#94](https://github.com/hjwylde/werewolf/issues/94))
* Added the Devoted Servant role. ([#47](https://github.com/hjwylde/werewolf/issues/47))
* Added `--force` flag to `end`. ([#77](https://github.com/hjwylde/werewolf/issues/77))

### v0.4.8.0

*Minor*

* Added role allocations to the game over messages. ([#27](https://github.com/hjwylde/werewolf/issues/27))

*Revisions*

* Improved prompt to Wolf-hound on how to choose an allegiance. ([#90](https://github.com/hjwylde/werewolf/issues/90))
* Changed Scapegoat's balance to 0. ([#91](https://github.com/hjwylde/werewolf/issues/91))
* Grouped `help commands` to improve readability. ([#97](https://github.com/hjwylde/werewolf/issues/97))
* Changed the `status` and `ping` commands to tell the caller when the game is over. ([#89](https://github.com/hjwylde/werewolf/issues/89))
* Added roles in game to the `status` command. ([#93](https://github.com/hjwylde/werewolf/issues/93))

### v0.4.7.1

*Revisions*

* Fixed bug where the Wolf-hound's turn messages would be displayed on every round. ([#87](https://github.com/hjwylde/werewolf/issues/87))
* Fixed a bug causing the Angel's joining Villagers message to be shown every round. ([#95](https://github.com/hjwylde/werewolf/issues/95))
* Fixed Village Idiot text to have spaces around the name. ([#87](https://github.com/hjwylde/werewolf/issues/87))
* Fixed a bug where the Werewolves couldn't win if it was down to 1 Werewolf and the Village Idiot. ([#88](https://github.com/hjwylde/werewolf/issues/88))

### v0.4.7.0

*Revisions*

* Fixed balance calculation to ensure total balance is between -2 and 2.
* Changed `--random-extra-roles` to have between `n / 3` and `n / 3 + 2` extra roles.
* Added prisms and traversals to Role, Player & Game modules. ([#20](https://github.com/hjwylde/werewolf/issues/20))
* Removed fudging of roles and replaced with fudging of allegiances.
* Wolf-hound now has their allegiance hidden when they are lynched.
* Fixed the grammar on the first Werewolves' turn messages.
* Moved Wolf-hound's turn to before the Seer's so that the Seer may see his allegiance properly.
* Restricted specifying `Simple Villager` or `Simple Werewolf` as extra roles.

### v0.4.6.1

*Revisions*

* Fixed Village Idiot text to have spaces around the name. ([#87](https://github.com/hjwylde/werewolf/issues/87))
* Fixed a bug where the Werewolves couldn't win if it was down to 1 Werewolf and the Village Idiot. ([#88](https://github.com/hjwylde/werewolf/issues/88))

### v0.4.6.0

*Minor*

* Added the Village Idiot role. ([#41](https://github.com/hjwylde/werewolf/issues/41))
* Added the Scapegoat's ability to choose whom may vote on the next day when he is blamed. ([#62](https://github.com/hjwylde/werewolf/issues/62))
* Added in balance concept for roles to help balance role selection. ([#81](https://github.com/hjwylde/werewolf/issues/81))
* Added `--random-extra-roles` option to `start`. ([#30](https://github.com/hjwylde/werewolf/issues/30))
* Added the Bear Tamer role. ([#45](https://github.com/hjwylde/werewolf/issues/45))
* Added a `circle` command. ([#45](https://github.com/hjwylde/werewolf/issues/45))

*Revisions*

* Fixed the Defender being unable to protect himself.
* Restructured library modules to only export relevant functions. ([#11](https://github.com/hjwylde/werewolf/issues/11))

### v0.4.5.0

*Minor*

* Added the Wolf-hound role. ([#50](https://github.com/hjwylde/werewolf/issues/50))
* Added a `version` command. ([#84](https://github.com/hjwylde/werewolf/issues/84))
* Added the Wild-child role. ([#49](https://github.com/hjwylde/werewolf/issues/49))
* Added the Angel role. ([#52](https://github.com/hjwylde/werewolf/issues/52))

*Revisions*

* Renamed the Villager role to Simple Villager.
* Renamed the Werewolf role to Simple Werewolf.
* Renamed the devourVoteCommand and lynchVoteCommand to voteDevourCommand and voteLynchCommand. ([#49](https://github.com/hjwylde/werewolf/issues/49))
* Fixed `quit` to advance the stage when the only role for that stage has quit.

### v0.4.4.1

*Revisions*

* Fixed grammar for the `currentStageMessages`. ([#83](https://github.com/hjwylde/werewolf/issues/83))
* Fixed the `heal` command help message to not require a `PLAYER` argument. ([#82](https://github.com/hjwylde/werewolf/issues/82))

### v0.4.4.0

*Minor*

* Removed `playerHealedMessage` and replaced with the generic `noPlayerDevouredMessage`. ([#80](https://github.com/hjwylde/werewolf/issues/80))
* Removed `playerProtectedMessage` and replaced with the generic `noPlayerDevouredMessage`. ([#80](https://github.com/hjwylde/werewolf/issues/80))

*Revisions*

* Privatised underscore methods and changed old uses to using lens. ([#20](https://github.com/hjwylde/werewolf/issues/20))
* Tidied up arbitrary instances by using `newtype`'s. ([#78](https://github.com/hjwylde/werewolf/issues/78))
* Fixed the `noPlayerDevouredMessage` to be displayed after sunrise. ([#80](https://github.com/hjwylde/werewolf/issues/80))
* Removed `Show` instance for `Command` and used `Blind`. ([#78](https://github.com/hjwylde/werewolf/issues/78))

### v0.4.3.2

*Revisions*

* Fixed grammar for the `currentStageMessages`. ([#83](https://github.com/hjwylde/werewolf/issues/83))
* Fixed the `heal` command help message to not require a `PLAYER` argument. ([#82](https://github.com/hjwylde/werewolf/issues/82))

### v0.4.3.1

*Revisions*

* Added missing module to Cabal file.

### v0.4.3.0

*Minor*

* Added the Defender role. ([#38](https://github.com/hjwylde/werewolf/issues/38))

### v0.4.2.3

*Revisions*

* Fixed grammar for the `currentStageMessages`. ([#83](https://github.com/hjwylde/werewolf/issues/83))
* Fixed the `heal` command help message to not require a `PLAYER` argument. ([#82](https://github.com/hjwylde/werewolf/issues/82))

### v0.4.2.2

*Revisions*

* Added missing module to Cabal file.

### v0.4.2.1

*Revisions*

* Fixed a bug causing the Witch being unable to heal themselves. ([#76](https://github.com/hjwylde/werewolf/issues/76))

### v0.4.2.0

*Minor*

* Added the Villager-Villager role. ([#37](https://github.com/hjwylde/werewolf/issues/37))

### v0.4.1.3

*Revisions*

* Fixed grammar for the `currentStageMessages`. ([#83](https://github.com/hjwylde/werewolf/issues/83))
* Fixed the `heal` command help message to not require a `PLAYER` argument. ([#82](https://github.com/hjwylde/werewolf/issues/82))

### v0.4.1.2

*Revisions*

* Added missing module to Cabal file.

### v0.4.1.1

*Revisions*

* Fixed a bug causing the Witch being unable to heal themselves. ([#76](https://github.com/hjwylde/werewolf/issues/76))

### v0.4.1.0

*Minor*

* Added the Witch role. ([#5](https://github.com/hjwylde/werewolf/issues/5))

### v0.4.0.1

*Revisions*

* Fixed grammar for the `currentStageMessages`. ([#83](https://github.com/hjwylde/werewolf/issues/83))

### v0.4.0.0

*Major*

* Restricted count of special roles to 1. ([#32](https://github.com/hjwylde/werewolf/issues/32))
* Changed private message structure to only ever be for a single player. ([#21](https://github.com/hjwylde/werewolf/issues/21))

*Minor*

* Renamed `turn` to `stage`. ([#70](https://github.com/hjwylde/werewolf/issues/70))
* Renamed `VillagersTurn` to `VillagesTurn`. ([#70](https://github.com/hjwylde/werewolf/issues/70))
* Added `events` to the game state. ([#71](https://github.com/hjwylde/werewolf/issues/71))
* Added private pinging to the `ping` command. ([#69](https://github.com/hjwylde/werewolf/issues/69))
* Restricted `end` to players in the current game. ([#74](https://github.com/hjwylde/werewolf/issues/74))

### v0.3.4.0

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

### v0.3.3.2

*Revisions*

* Fixed missing file in Cabal file. ([#18](https://github.com/hjwylde/werewolf/issues/18))

### v0.3.3.1

*Revisions*

* Added `noIntersperse` to `interpret`. ([#60](https://github.com/hjwylde/werewolf/issues/60))

### v0.3.3.0

*Minor*

* Added a Scapegoat role. ([#40](https://github.com/hjwylde/werewolf/issues/40))
* Added a `status` command. ([#18](https://github.com/hjwylde/werewolf/issues/18))

*Revisions*

* Added `--` to help description of `interpret`. ([#60](https://github.com/hjwylde/werewolf/issues/60))

### v0.3.2.0

*Minor*

* Added a "Whom would you like to lynch?" message during the Villagers' turn. ([#25](https://github.com/hjwylde/werewolf/issues/25))
* Allowed lowercase roles for `--extra-roles` in the `start` command. ([#33](https://github.com/hjwylde/werewolf/issues/33))

*Revisions*

* Shrunk some of the help text to make it more readable. ([#25](https://github.com/hjwylde/werewolf/issues/25))

### v0.3.1.3

*Revisions*

* Fixed a bug where Werewolves could devour other Werewolves. ([#34](https://github.com/hjwylde/werewolf/issues/34))
* Changed Werewolf text from "kill" to "devour". ([#34](https://github.com/hjwylde/werewolf/issues/34))

### v0.3.1.2

*Revisions*

* Fixed dead werewolves being informed of votes. ([#24](https://github.com/hjwylde/werewolf/issues/24))

### v0.3.1.1

*Revisions*

* Tidied up the help text to be smaller. ([#26](https://github.com/hjwylde/werewolf/issues/26))
* Fixed a bug where the turn was advanced to Werewolves when no Werewolves were alive. ([#26](https://github.com/hjwylde/werewolf/issues/26))

### v0.3.1.0

*Minor*

* Added a message to say the names of all the players at the start of a game. ([#23](https://github.com/hjwylde/werewolf/issues/23))
* Added a message to say the roles in play at the start of a game. ([#16](https://github.com/hjwylde/werewolf/issues/16))

### v0.3.0.5

*Revisions*

* Fixed a bug where Werewolves could devour other Werewolves. ([#34](https://github.com/hjwylde/werewolf/issues/34))
* Changed Werewolf text from "kill" to "devour". ([#34](https://github.com/hjwylde/werewolf/issues/34))

### v0.3.0.4

*Revisions*

* Fixed dead werewolves being informed of votes. ([#24](https://github.com/hjwylde/werewolf/issues/24))

### v0.3.0.3

*Revisions*

* Tidied up the help text to be smaller. ([#26](https://github.com/hjwylde/werewolf/issues/26))
* Fixed a bug where the turn was advanced to Werewolves when no Werewolves were alive. ([#26](https://github.com/hjwylde/werewolf/issues/26))

### v0.3.0.2

*Revisions*

* Tidied up the help text to be smaller. ([#26](https://github.com/hjwylde/werewolf/issues/26))
* Fixed a bug where the turn was advanced to Werewolves when no Werewolves were alive. ([#26](https://github.com/hjwylde/werewolf/issues/26))

### v0.3.0.1

*Revisions*

* Fixed `interpret` to display the commands help messages when given invalid arguments. ([#22](https://github.com/hjwylde/werewolf/issues/22))

### v0.3.0.0

*Major*

* Added `--extra-roles` option to `start`. ([#12](https://github.com/hjwylde/werewolf/issues/12))
* Removed Seer from being included by default. ([#12](https://github.com/hjwylde/werewolf/issues/12))

*Minor*

* Allowed `start` to work when the game has ended but `end` hasn't been called. ([#15](https://github.com/hjwylde/werewolf/issues/15))
* Added `quit` command. ([#13](https://github.com/hjwylde/werewolf/issues/13))

### v0.2.0.2

*Revisions*

* Fixed dead werewolves being informed of votes. ([#24](https://github.com/hjwylde/werewolf/issues/24))

### v0.2.0.1

*Revisions*

* Tidied up the help text to be smaller. ([#26](https://github.com/hjwylde/werewolf/issues/26))
* Fixed a bug where the turn was advanced to Werewolves when no Werewolves were alive. ([#26](https://github.com/hjwylde/werewolf/issues/26))

### v0.2.0.0

*Major*

* Added the Seer role. ([#4](https://github.com/hjwylde/werewolf/issues/4))
* Removed the need to encode / decode to JSON for the state file. ([#9](https://github.com/hjwylde/werewolf/issues/9))

### v0.1.0.0

*Major*

* Initial implementation with Villagers and Werewolves. ([#1](https://github.com/hjwylde/werewolf/issues/1))
