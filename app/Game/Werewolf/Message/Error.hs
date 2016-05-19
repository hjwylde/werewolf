{-|
Module      : Game.Werewolf.Message.Error
Description : Suite of error messages used throughout the game.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A 'Message' is used to relay information back to either all players or a single player. This module
defines suite of error messages used throughout the werewolf game.
-}

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

import Data.Text (Text)

import Game.Werewolf.Player
import Game.Werewolf.Response
import Game.Werewolf.Role
import Game.Werewolf.Variant.Standard.Error

playerHasAlreadyVotedToBootMessage :: Text -> Player -> Message
playerHasAlreadyVotedToBootMessage to = privateMessage to . callerAlreadyVotedBootText

playerCannotChooseSelfMessage :: Text -> Message
playerCannotChooseSelfMessage to = privateMessage to callerCannotChooseSelfText

playerCannotChooseJesterMessage :: Text -> Message
playerCannotChooseJesterMessage to = privateMessage to callerCannotChooseJesterText

playerMustChooseAtLeastOneTargetMessage :: Text -> Message
playerMustChooseAtLeastOneTargetMessage to = privateMessage to noTargetText

noGameRunningMessage :: Text -> Message
noGameRunningMessage to = privateMessage to noGameRunningText

playerCannotDoThatMessage :: Text -> Message
playerCannotDoThatMessage to = privateMessage to callerCannotDoThatText

playerCannotDoThatRightNowMessage :: Text -> Message
playerCannotDoThatRightNowMessage to = privateMessage to callerCannotDoThatRightNowText

playerIsDeadMessage :: Text -> Message
playerIsDeadMessage to = privateMessage to callerDeadText

playerDoesNotExistMessage :: Text -> Text -> Message
playerDoesNotExistMessage to = privateMessage to . playerDoesNotExistText

targetIsDeadMessage :: Text -> Player -> Message
targetIsDeadMessage to = privateMessage to . targetDeadText

playerHasAlreadyHealedMessage :: Text -> Message
playerHasAlreadyHealedMessage to = privateMessage to callerAlreadyHealedText

playerHasAlreadyPoisonedMessage :: Text -> Message
playerHasAlreadyPoisonedMessage to = privateMessage to callerAlreadyPoisonedText

playerCannotProtectSamePlayerTwiceInARowMessage :: Text -> Message
playerCannotProtectSamePlayerTwiceInARowMessage to = privateMessage to callerCannotProtectSamePlayerText

gameAlreadyRunningMessage :: Text -> Message
gameAlreadyRunningMessage to = privateMessage to gameAlreadyRunningText

mustHaveAtLeast7PlayersMessage :: Text -> Message
mustHaveAtLeast7PlayersMessage to = privateMessage to playerCountTooLowText

playerNamesMustBeUniqueMessage :: Text -> Message
playerNamesMustBeUniqueMessage to = privateMessage to playerNamesNotUniqueText

roleCountRestrictedMessage :: Text -> Role -> Message
roleCountRestrictedMessage to = privateMessage to . roleCountRestrictedText

roleDoesNotExistMessage :: Text -> Text -> Message
roleDoesNotExistMessage to = privateMessage to . roleDoesNotExistText

playerHasNotVotedMessage :: Text -> Message
playerHasNotVotedMessage to = privateMessage to callerNotVotedText

playerHasAlreadyVotedMessage :: Text -> Message
playerHasAlreadyVotedMessage to = privateMessage to callerAlreadyVotedText

playerCannotDevourAnotherWerewolfMessage :: Text -> Message
playerCannotDevourAnotherWerewolfMessage to = privateMessage to callerCannotDevourAnotherWerewolfText
