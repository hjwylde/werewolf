{-|
Module      : Game.Werewolf.Message.Error
Description : Suite of error messages used throughout the game.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A 'Message' is used to relay information back to either all players or a single player. This module
defines suite of error messages used throughout the werewolf game.
-}

{-# LANGUAGE QuasiQuotes #-}

module Game.Werewolf.Message.Error (
    -- * Command

    -- ** Boot
    playerHasAlreadyVotedToBootMessage,

    -- ** Choose
    playerCannotChooseSelfMessage, playerCannotChooseJesterMessage,
    playerMustChooseAtLeastOneTargetMessage,

    -- ** General
    noGameRunningMessage, playerCannotDoThatMessage, playerCannotDoThatRightNowMessage,
    playerIsDeadMessage, playerDoesNotExistMessage, targetIsDeadMessage,

    -- ** Heal
    playerHasAlreadyHealedMessage,

    -- ** Poison
    playerHasAlreadyPoisonedMessage,

    -- ** Protect
    playerCannotProtectSamePlayerTwiceInARowMessage,

    -- ** Start
    gameAlreadyRunningMessage, mustHaveAtLeast7PlayersMessage, playerNamesMustBeUniqueMessage,
    roleCountRestrictedMessage, roleDoesNotExistMessage,

    -- ** Unvote
    playerHasNotVotedMessage,

    -- ** Vote
    playerHasAlreadyVotedMessage, playerCannotDevourAnotherWerewolfMessage,
) where

import Data.String.Humanise
import Data.String.Interpolate.Extra
import Data.Text                     (Text)

import Game.Werewolf.Player
import Game.Werewolf.Response
import Game.Werewolf.Role     hiding (name)

playerHasAlreadyVotedToBootMessage :: Text -> Player -> Message
playerHasAlreadyVotedToBootMessage to target = privateMessage to [iFile|variant/standard/error/command/boot/player-already-voted-boot.text|]

playerCannotChooseSelfMessage :: Text -> Message
playerCannotChooseSelfMessage to = privateMessage to [iFile|variant/standard/error/command/choose/player-cannot-choose-self.text|]

playerCannotChooseJesterMessage :: Text -> Message
playerCannotChooseJesterMessage to = privateMessage to [iFile|variant/standard/error/command/choose/player-cannot-choose-jester.text|]

playerMustChooseAtLeastOneTargetMessage :: Text -> Message
playerMustChooseAtLeastOneTargetMessage to = privateMessage to [iFile|variant/standard/error/command/choose/player-must-choose-at-least-one-target.text|]

noGameRunningMessage :: Text -> Message
noGameRunningMessage to = privateMessage to [iFile|variant/standard/error/command/general/no-game-running.text|]

playerCannotDoThatMessage :: Text -> Message
playerCannotDoThatMessage to = privateMessage to [iFile|variant/standard/error/command/general/player-cannot-do-that.text|]

playerCannotDoThatRightNowMessage :: Text -> Message
playerCannotDoThatRightNowMessage to = privateMessage to [iFile|variant/standard/error/command/general/player-cannot-do-that-right-now.text|]

playerIsDeadMessage :: Text -> Message
playerIsDeadMessage to = privateMessage to [iFile|variant/standard/error/command/general/player-dead.text|]

playerDoesNotExistMessage :: Text -> Text -> Message
playerDoesNotExistMessage to name = privateMessage to [iFile|variant/standard/error/command/general/player-does-not-exist.text|]

targetIsDeadMessage :: Text -> Player -> Message
targetIsDeadMessage to target = privateMessage to [iFile|variant/standard/error/command/general/target-dead.text|]

playerHasAlreadyHealedMessage :: Text -> Message
playerHasAlreadyHealedMessage to = privateMessage to [iFile|variant/standard/error/command/heal/player-already-healed.text|]

playerHasAlreadyPoisonedMessage :: Text -> Message
playerHasAlreadyPoisonedMessage to = privateMessage to [iFile|variant/standard/error/command/poison/player-already-poisoned.text|]

playerCannotProtectSamePlayerTwiceInARowMessage :: Text -> Message
playerCannotProtectSamePlayerTwiceInARowMessage to = privateMessage to [iFile|variant/standard/error/command/protect/player-cannot-protect-same-player-twice-in-a-row.text|]

gameAlreadyRunningMessage :: Text -> Message
gameAlreadyRunningMessage to = privateMessage to [iFile|variant/standard/error/command/start/game-already-running.text|]

mustHaveAtLeast7PlayersMessage :: Text -> Message
mustHaveAtLeast7PlayersMessage to = privateMessage to [iFile|variant/standard/error/command/start/must-have-at-least-7-players.text|]

playerNamesMustBeUniqueMessage :: Text -> Message
playerNamesMustBeUniqueMessage to = privateMessage to [iFile|variant/standard/error/command/start/player-names-must-be-unique.text|]

roleCountRestrictedMessage :: Text -> Role -> Message
roleCountRestrictedMessage to role = privateMessage to [iFile|variant/standard/error/command/start/role-count-restricted.text|]

roleDoesNotExistMessage :: Text -> Text -> Message
roleDoesNotExistMessage to roleName = privateMessage to [iFile|variant/standard/error/command/start/role-does-not-exist.text|]

playerHasNotVotedMessage :: Text -> Message
playerHasNotVotedMessage to = privateMessage to [iFile|variant/standard/error/command/unvote/player-not-voted.text|]

playerHasAlreadyVotedMessage :: Text -> Message
playerHasAlreadyVotedMessage to = privateMessage to [iFile|variant/standard/error/command/vote/player-already-voted.text|]

playerCannotDevourAnotherWerewolfMessage :: Text -> Message
playerCannotDevourAnotherWerewolfMessage to = privateMessage to [iFile|variant/standard/error/command/vote/player-cannot-devour-werewolf.text|]
