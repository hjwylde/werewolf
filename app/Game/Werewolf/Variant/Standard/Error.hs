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
    callerCannotChooseJesterText, callerCannotChooseSelfText, callerCannotChooseZombieText,
    noTargetText,

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

import Game.Werewolf

callerAlreadyVotedBootText :: Player -> Text
callerAlreadyVotedBootText target = [iFile|variant/standard/error/command/boot/caller-already-voted-boot.txt|]

callerCannotChooseJesterText :: Text
callerCannotChooseJesterText = [iFile|variant/standard/error/command/choose/caller-cannot-choose-jester.txt|]

callerCannotChooseSelfText :: Text
callerCannotChooseSelfText = [iFile|variant/standard/error/command/choose/caller-cannot-choose-self.txt|]

callerCannotChooseZombieText :: Text
callerCannotChooseZombieText = [iFile|variant/standard/error/command/choose/caller-cannot-choose-zombie.txt|]

noTargetText :: Text
noTargetText = [iFile|variant/standard/error/command/choose/no-target.txt|]

callerCannotDoThatText :: Text
callerCannotDoThatText = [iFile|variant/standard/error/command/general/caller-cannot-do-that.txt|]

callerCannotDoThatRightNowText :: Text
callerCannotDoThatRightNowText = [iFile|variant/standard/error/command/general/caller-cannot-do-that-right-now.txt|]

callerDeadText :: Text
callerDeadText = [iFile|variant/standard/error/command/general/caller-dead.txt|]

noGameRunningText :: Text
noGameRunningText = [iFile|variant/standard/error/command/general/no-game-running.txt|]

playerDoesNotExistText :: Text -> Text
playerDoesNotExistText name = [iFile|variant/standard/error/command/general/player-does-not-exist.txt|]

targetDeadText :: Player -> Text
targetDeadText target = [iFile|variant/standard/error/command/general/target-dead.txt|]

callerAlreadyHealedText :: Text
callerAlreadyHealedText = [iFile|variant/standard/error/command/heal/caller-already-healed.txt|]

callerAlreadyPoisonedText :: Text
callerAlreadyPoisonedText = [iFile|variant/standard/error/command/poison/caller-already-poisoned.txt|]

callerCannotProtectSamePlayerText :: Text
callerCannotProtectSamePlayerText = [iFile|variant/standard/error/command/protect/caller-cannot-protect-same-player.txt|]

gameAlreadyRunningText :: Text
gameAlreadyRunningText = [iFile|variant/standard/error/command/start/game-already-running.txt|]

playerCountTooLowText :: Text
playerCountTooLowText = [iFile|variant/standard/error/command/start/player-count-too-low.txt|]

playerNamesNotUniqueText :: Text
playerNamesNotUniqueText = [iFile|variant/standard/error/command/start/player-names-not-unique.txt|]

roleCountRestrictedText :: Role -> Text
roleCountRestrictedText role = [iFile|variant/standard/error/command/start/role-count-restricted.txt|]

roleDoesNotExistText :: Text -> Text
roleDoesNotExistText roleName = [iFile|variant/standard/error/command/start/role-does-not-exist.txt|]

callerNotVotedText :: Text
callerNotVotedText = [iFile|variant/standard/error/command/unvote/caller-not-voted.txt|]

callerAlreadyVotedText :: Text
callerAlreadyVotedText = [iFile|variant/standard/error/command/vote/caller-already-voted.txt|]

callerCannotDevourAnotherWerewolfText :: Text
callerCannotDevourAnotherWerewolfText = [iFile|variant/standard/error/command/vote/caller-cannot-devour-werewolf.txt|]
