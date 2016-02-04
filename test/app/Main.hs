{-|
Module      : Main
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Main (
    main
) where

import Game.Werewolf.Test.Arbitrary ()
import Game.Werewolf.Test.Command
import Game.Werewolf.Test.Engine
import Game.Werewolf.Test.Game
import Game.Werewolf.Test.Player

import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = return $ testGroup "Tests" (concat
    [allCommandTests, allEngineTests, allGameTests, allPlayerTests]
    )

allCommandTests :: [TestTree]
allCommandTests =
    [ testProperty "devour vote command errors when game is over"           prop_devourVoteCommandErrorsWhenGameIsOver
    , testProperty "devour vote command errors when caller does not exist"  prop_devourVoteCommandErrorsWhenCallerDoesNotExist
    , testProperty "devour vote command errors when target does not exist"  prop_devourVoteCommandErrorsWhenTargetDoesNotExist
    , testProperty "devour vote command errors when caller is dead"         prop_devourVoteCommandErrorsWhenCallerIsDead
    , testProperty "devour vote command errors when target is dead"         prop_devourVoteCommandErrorsWhenTargetIsDead
    , testProperty "devour vote command errors when not werewolves turn"    prop_devourVoteCommandErrorsWhenNotWerewolvesTurn
    , testProperty "devour vote command errors when caller not werewolf"    prop_devourVoteCommandErrorsWhenCallerNotWerewolf
    , testProperty "devour vote command errors when caller has voted"       prop_devourVoteCommandErrorsWhenCallerHasVoted
    , testProperty "devour vote command errors when target werewolf"        prop_devourVoteCommandErrorsWhenTargetWerewolf
    , testProperty "devour vote command updates votes"                      prop_devourVoteCommandUpdatesVotes

    , testProperty "heal command errors when game is over"          prop_healCommandErrorsWhenGameIsOver
    , testProperty "heal command errors when caller does not exist" prop_healCommandErrorsWhenCallerDoesNotExist
    , testProperty "heal command errors when caller is dead"        prop_healCommandErrorsWhenCallerIsDead
    , testProperty "heal command errors when no target is devoured" prop_healCommandErrorsWhenNoTargetIsDevoured
    , testProperty "heal command errors when not witch's turn"      prop_healCommandErrorsWhenNotWitchsTurn
    , testProperty "heal command errors when caller has healed"     prop_healCommandErrorsWhenCallerHasHealed
    , testProperty "heal command errors when caller not witch"      prop_healCommandErrorsWhenCallerNotWitch
    , testProperty "heal command sets heal"                         prop_healCommandSetsHeal
    , testProperty "heal command sets heal used"                    prop_healCommandSetsHealUsed

    , testProperty "lynch vote command errors when game is over"            prop_lynchVoteCommandErrorsWhenGameIsOver
    , testProperty "lynch vote command errors when caller does not exist"   prop_lynchVoteCommandErrorsWhenCallerDoesNotExist
    , testProperty "lynch vote command errors when target does not exist"   prop_lynchVoteCommandErrorsWhenTargetDoesNotExist
    , testProperty "lynch vote command errors when caller is dead"          prop_lynchVoteCommandErrorsWhenCallerIsDead
    , testProperty "lynch vote command errors when target is dead"          prop_lynchVoteCommandErrorsWhenTargetIsDead
    , testProperty "lynch vote command errors when not villages turn"       prop_lynchVoteCommandErrorsWhenNotVillagesTurn
    , testProperty "lynch vote command errors when caller has voted"        prop_lynchVoteCommandErrorsWhenCallerHasVoted
    , testProperty "lynch vote command updates votes"                       prop_lynchVoteCommandUpdatesVotes

    , testProperty "pass command errors when game is over"          prop_passCommandErrorsWhenGameIsOver
    , testProperty "pass command errors when caller does not exist" prop_passCommandErrorsWhenCallerDoesNotExist
    , testProperty "pass command errors when caller is dead"        prop_passCommandErrorsWhenCallerIsDead
    , testProperty "pass command errors when not witch's turn"      prop_passCommandErrorsWhenNotWitchsTurn
    , testProperty "pass command updates passes"                    prop_passCommandUpdatesPasses

    , testProperty "poison command errors when game is over"            prop_poisonCommandErrorsWhenGameIsOver
    , testProperty "poison command errors when caller does not exist"   prop_poisonCommandErrorsWhenCallerDoesNotExist
    , testProperty "poison command errors when target does not exist"   prop_poisonCommandErrorsWhenTargetDoesNotExist
    , testProperty "poison command errors when caller is dead"          prop_poisonCommandErrorsWhenCallerIsDead
    , testProperty "poison command errors when target is dead"          prop_poisonCommandErrorsWhenTargetIsDead
    , testProperty "poison command errors when target is devoured"      prop_poisonCommandErrorsWhenTargetIsDevoured
    , testProperty "poison command errors when not witch's turn"        prop_poisonCommandErrorsWhenNotWitchsTurn
    , testProperty "poison command errors when caller has poisoned"     prop_poisonCommandErrorsWhenCallerHasPoisoned
    , testProperty "poison command errors when caller not witch"        prop_poisonCommandErrorsWhenCallerNotWitch
    , testProperty "poison command sets poison"                         prop_poisonCommandSetsPoison
    , testProperty "poison command sets poison used"                    prop_poisonCommandSetsPoisonUsed

    , testProperty "protect command errors when game is over"               prop_protectCommandErrorsWhenGameIsOver
    , testProperty "protect command errors when caller does not exist"      prop_protectCommandErrorsWhenCallerDoesNotExist
    , testProperty "protect command errors when target does not exist"      prop_protectCommandErrorsWhenTargetDoesNotExist
    , testProperty "protect command errors when caller is dead"             prop_protectCommandErrorsWhenCallerIsDead
    , testProperty "protect command errors when target is dead"             prop_protectCommandErrorsWhenTargetIsDead
    , testProperty "protect command errors when not defender's turn"        prop_protectCommandErrorsWhenNotDefendersTurn
    , testProperty "protect command errors when caller not defender"        prop_protectCommandErrorsWhenCallerNotDefender
    , testProperty "protect command errors when target is caller"           prop_protectCommandErrorsWhenTargetIsCaller
    , testProperty "protect command errors when target is prior protect"    prop_protectCommandErrorsWhenTargetIsPriorProtect
    , testProperty "protect command sets prior protect"                     prop_protectCommandSetsPriorProtect
    , testProperty "protect command sets protect"                           prop_protectCommandSetsProtect

    , testProperty "quit command errors when game is over"                      prop_quitCommandErrorsWhenGameIsOver
    , testProperty "quit command errors when caller does not exist"             prop_quitCommandErrorsWhenCallerDoesNotExist
    , testProperty "quit command errors when caller is dead"                    prop_quitCommandErrorsWhenCallerIsDead
    , testProperty "quit command kills player"                                  prop_quitCommandKillsPlayer
    , testProperty "quit command clears heal when caller is witch"              prop_quitCommandClearsHealWhenCallerIsWitch
    , testProperty "quit command clears heal used when caller is witch"         prop_quitCommandClearsHealUsedWhenCallerIsWitch
    , testProperty "quit command clears poison when caller is witch"            prop_quitCommandClearsPoisonWhenCallerIsWitch
    , testProperty "quit command clears poison used when caller is witch"       prop_quitCommandClearsPoisonUsedWhenCallerIsWitch
    , testProperty "quit command clears prior protect when caller is defender"  prop_quitCommandClearsPriorProtectWhenCallerIsDefender
    , testProperty "quit command clears protect when caller is defender"        prop_quitCommandClearsProtectWhenCallerIsDefender
    , testProperty "quit command clears player's devour vote"                   prop_quitCommandClearsPlayersDevourVote
    , testProperty "quit command clears player's lynch vote"                    prop_quitCommandClearsPlayersLynchVote

    , testProperty "see command errors when game is over"           prop_seeCommandErrorsWhenGameIsOver
    , testProperty "see command errors when caller does not exist"  prop_seeCommandErrorsWhenCallerDoesNotExist
    , testProperty "see command errors when target does not exist"  prop_seeCommandErrorsWhenTargetDoesNotExist
    , testProperty "see command errors when caller is dead"         prop_seeCommandErrorsWhenCallerIsDead
    , testProperty "see command errors when target is dead"         prop_seeCommandErrorsWhenTargetIsDead
    , testProperty "see command errors when not seer's turn"        prop_seeCommandErrorsWhenNotSeersTurn
    , testProperty "see command errors when caller not seer"        prop_seeCommandErrorsWhenCallerNotSeer
    , testProperty "see command sets see"                           prop_seeCommandSetsSee
    ]

allEngineTests :: [TestTree]
allEngineTests =
    [ testProperty "check stage skips defender's turn when no defender" prop_checkStageSkipsDefendersTurnWhenNoDefender
    , testProperty "check stage skips seer's turn when no seer"         prop_checkStageSkipsSeersTurnWhenNoSeer
    , testProperty "check stage skips witch's turn when no witch"       prop_checkStageSkipsWitchsTurnWhenNoWitch
    , testProperty "check stage does nothing when game over"            prop_checkStageDoesNothingWhenGameOver

    , testProperty "check defender's turn advances to werewolves' turn" prop_checkDefendersTurnAdvancesToWerewolvesTurn

    , testProperty "check seer's turn advances to defender's turn"  prop_checkSeersTurnAdvancesToDefendersTurn
    , testProperty "check seer's turn resets sees"                  prop_checkSeersTurnResetsSee
    , testProperty "check seer's turn does nothing unless seen"     prop_checkSeersTurnDoesNothingUnlessSeen

    , testProperty "check villages' turn advances to seer's turn"                           prop_checkVillagesTurnAdvancesToSeersTurn
    , testProperty "check villages' turn lynches one player when consensus"                 prop_checkVillagesTurnLynchesOnePlayerWhenConsensus
    , testProperty "check villages' turn lynches no one when conflicted and no scapegoats"  prop_checkVillagesTurnLynchesNoOneWhenConflictedAndNoScapegoats
    , testProperty "check villages' turn lynches scapegoat when conflicted"                 prop_checkVillagesTurnLynchesScapegoatWhenConflicted
    , testProperty "check villages' turn resets votes"                                      prop_checkVillagesTurnResetsVotes
    , testProperty "check villages' turn does nothing unless all voted"                     prop_checkVillagesTurnDoesNothingUnlessAllVoted

    , testProperty "check werewolves' turn advances to witch's turn"                    prop_checkWerewolvesTurnAdvancesToWitchsTurn
    , testProperty "check werewolves' turn doesn't skip witch's turn when witch devoured" prop_checkWerewolvesTurnDoesntSkipWitchsTurnWhenWitchDevoured
    , testProperty "check werewolves' turn skips witch's turn when healed and poisoned" prop_checkWerewolvesTurnSkipsWitchsTurnWhenHealedAndPoisoned
    , testProperty "check werewolves' turn kills one player when consensus"             prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus
    , testProperty "check werewolves' turn kills no one when conflicted"                prop_checkWerewolvesTurnKillsNoOneWhenConflicted
    , testProperty "check werewolves' turn kills no one when target defended"           prop_checkWerewolvesTurnKillsNoOneWhenTargetDefended
    , testProperty "check werewolves' turn resets protect"                              prop_checkWerewolvesTurnResetsProtect
    , testProperty "check werewolves' turn resets votes"                                prop_checkWerewolvesTurnResetsVotes
    , testProperty "check werewolves' turn does nothing unless all voted"               prop_checkWerewolvesTurnDoesNothingUnlessAllVoted

    , testProperty "check witch's turn advances to villages' turn"      prop_checkWitchsTurnAdvancesToVillagesTurn
    , testProperty "check witch's turn heals devouree when healed"      prop_checkWitchsTurnHealsDevoureeWhenHealed
    , testProperty "check witch's turn kills one player when poisoned"  prop_checkWitchsTurnKillsOnePlayerWhenPoisoned
    , testProperty "check witch's turn does nothing when passed"        prop_checkWitchsTurnDoesNothingWhenPassed
    , testProperty "check witch's turn resets heal"                     prop_checkWitchsTurnResetsHeal
    , testProperty "check witch's turn resets poison"                   prop_checkWitchsTurnResetsPoison

    , testProperty "check game over advances stage"                                     prop_checkGameOverAdvancesStage
    , testProperty "check game over does nothing when at least two allegiances alive"   prop_checkGameOverDoesNothingWhenAtLeastTwoAllegiancesAlive

    , testProperty "start game starts with sunset stage"                    prop_startGameStartsWithSunsetStage
    , testProperty "start game uses given players"                          prop_startGameUsesGivenPlayers
    , testProperty "start game errors unless unique player names"           prop_startGameErrorsUnlessUniquePlayerNames
    , testProperty "start game errors when less than 7 players"             prop_startGameErrorsWhenLessThan7Players
    , testProperty "start game errors when more than 24 players"            prop_startGameErrorsWhenMoreThan24Players
    , testProperty "start game errors when more than 1 defender"            prop_startGameErrorsWhenMoreThan1Defender
    , testProperty "start game errors when more than 1 scapegoat"           prop_startGameErrorsWhenMoreThan1Scapegoat
    , testProperty "start game errors when more than 1 seer"                prop_startGameErrorsWhenMoreThan1Seer
    , testProperty "start game errors when more than 1 villager-villager"   prop_startGameErrorsWhenMoreThan1VillagerVillager
    , testProperty "start game errors when more than 1 witch"               prop_startGameErrorsWhenMoreThan1Witch

    , testProperty "create players uses given player names" prop_createPlayersUsesGivenPlayerNames
    , testProperty "create players uses given roles"        prop_createPlayersUsesGivenRoles
    , testProperty "create players creates alive players"   prop_createPlayersCreatesAlivePlayers

    , testProperty "randomise roles returns n roles"            prop_randomiseRolesReturnsNRoles
    , testProperty "randomise roles uses given roles"           prop_randomiseRolesUsesGivenRoles
    , testProperty "randomise roles proportions allegiances"    prop_randomiseRolesProportionsAllegiances
    ]

allGameTests :: [TestTree]
allGameTests =
    [ testProperty "new game starts with sunset stage"      prop_newGameStartsWithSunsetStage
    , testProperty "new game starts with events empty"      prop_newGameStartsWithEventsEmpty
    , testProperty "new game starts with passes empty"      prop_newGameStartsWithPassesEmpty
    , testProperty "new game starts with no heal"           prop_newGameStartsWithNoHeal
    , testProperty "new game starts with no heal used"      prop_newGameStartsWithNoHealUsed
    , testProperty "new game starts with no poison"         prop_newGameStartsWithNoPoison
    , testProperty "new game starts with no poison used"    prop_newGameStartsWithNoPoisonUsed
    , testProperty "new game starts with no prior protect"  prop_newGameStartsWithNoPriorProtect
    , testProperty "new game starts with no protect"        prop_newGameStartsWithNoProtect
    , testProperty "new game starts with no see"            prop_newGameStartsWithNoSee
    , testProperty "new game starts with votes empty"       prop_newGameStartsWithVotesEmpty
    , testProperty "new game uses given players"            prop_newGameUsesGivenPlayers
    ]

allPlayerTests :: [TestTree]
allPlayerTests =
    [ testProperty "new player is alive" prop_newPlayerIsAlive
    ]
