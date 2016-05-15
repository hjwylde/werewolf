{-|
Module      : Game.Werewolf.Variant.Standard.Error
Description : Suite of error messages used throughout the game.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A 'Message' is used to relay information back to either all players or a single player. This module
defines suite of error messages used throughout the werewolf game for the 'Standard' variant.
-}

{-# LANGUAGE QuasiQuotes #-}

module Game.Werewolf.Variant.Standard.Error (
    -- * Command

    -- ** Boot
    callerAlreadyVotedBootText,

    -- ** Choose
    callerCannotChooseJesterText, callerCannotChooseSelfText, noTargetText,

    -- ** General
    callerCannotDoThatText, callerCannotDoThatRightNowText, callerDeadText, noGameRunningText,
    playerDoesNotExistText, targetDeadText,

    -- ** Heal
    callerAlreadyHealedText,

    -- ** Poison
    callerAlreadyPoisonedText,

    -- ** Protect
    callerCannotProtectSamePlayerText,

    -- ** Start
    gameAlreadyRunningText, playerCountTooLowText, playerNamesNotUniqueText,
    roleCountRestrictedText, roleDoesNotExistText,

    -- ** Unvote
    callerNotVotedText,

    -- ** Vote
    callerAlreadyVotedText, callerCannotDevourAnotherWerewolfText,
) where

import Data.String.Humanise
import Data.String.Interpolate.Extra
import Data.Text                     (Text)

import Game.Werewolf.Player
import Game.Werewolf.Role   hiding (name)

callerAlreadyVotedBootText :: Player -> Text
callerAlreadyVotedBootText target = [iFile|variant/standard/error/command/boot/caller-already-voted-boot.text|]

callerCannotChooseJesterText :: Text
callerCannotChooseJesterText = [iFile|variant/standard/error/command/choose/caller-cannot-choose-jester.text|]

callerCannotChooseSelfText :: Text
callerCannotChooseSelfText = [iFile|variant/standard/error/command/choose/caller-cannot-choose-self.text|]

noTargetText :: Text
noTargetText = [iFile|variant/standard/error/command/choose/no-target.text|]

callerCannotDoThatText :: Text
callerCannotDoThatText = [iFile|variant/standard/error/command/general/caller-cannot-do-that.text|]

callerCannotDoThatRightNowText :: Text
callerCannotDoThatRightNowText = [iFile|variant/standard/error/command/general/caller-cannot-do-that-right-now.text|]

callerDeadText :: Text
callerDeadText = [iFile|variant/standard/error/command/general/caller-dead.text|]

noGameRunningText :: Text
noGameRunningText = [iFile|variant/standard/error/command/general/no-game-running.text|]

playerDoesNotExistText :: Text -> Text
playerDoesNotExistText name = [iFile|variant/standard/error/command/general/player-does-not-exist.text|]

targetDeadText :: Player -> Text
targetDeadText target = [iFile|variant/standard/error/command/general/target-dead.text|]

callerAlreadyHealedText :: Text
callerAlreadyHealedText = [iFile|variant/standard/error/command/heal/caller-already-healed.text|]

callerAlreadyPoisonedText :: Text
callerAlreadyPoisonedText = [iFile|variant/standard/error/command/poison/caller-already-poisoned.text|]

callerCannotProtectSamePlayerText :: Text
callerCannotProtectSamePlayerText = [iFile|variant/standard/error/command/protect/caller-cannot-protect-same-player.text|]

gameAlreadyRunningText :: Text
gameAlreadyRunningText = [iFile|variant/standard/error/command/start/game-already-running.text|]

playerCountTooLowText :: Text
playerCountTooLowText = [iFile|variant/standard/error/command/start/player-count-too-low.text|]

playerNamesNotUniqueText :: Text
playerNamesNotUniqueText = [iFile|variant/standard/error/command/start/player-names-not-unique.text|]

roleCountRestrictedText :: Role -> Text
roleCountRestrictedText role = [iFile|variant/standard/error/command/start/role-count-restricted.text|]

roleDoesNotExistText :: Text -> Text
roleDoesNotExistText roleName = [iFile|variant/standard/error/command/start/role-does-not-exist.text|]

callerNotVotedText :: Text
callerNotVotedText = [iFile|variant/standard/error/command/unvote/caller-not-voted.text|]

callerAlreadyVotedText :: Text
callerAlreadyVotedText = [iFile|variant/standard/error/command/vote/caller-already-voted.text|]

callerCannotDevourAnotherWerewolfText :: Text
callerCannotDevourAnotherWerewolfText = [iFile|variant/standard/error/command/vote/caller-cannot-devour-werewolf.text|]
