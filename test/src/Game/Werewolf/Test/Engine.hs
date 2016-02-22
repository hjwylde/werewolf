{-|
Module      : Game.Werewolf.Test.Engine
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Engine (
    -- * Tests
    allEngineTests,
) where

import Control.Lens         hiding (elements)
import Control.Monad.Except
import Control.Monad.Writer

import           Data.Either.Extra
import           Data.List.Extra
import qualified Data.Map          as Map
import           Data.Maybe

import Game.Werewolf.Command
import Game.Werewolf.Engine
import Game.Werewolf.Internal.Game
import Game.Werewolf.Internal.Player
import Game.Werewolf.Internal.Role   hiding (name)
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Prelude hiding (round)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

allEngineTests :: [TestTree]
allEngineTests =
    [ testProperty "check stage skips defender's turn when no defender"         prop_checkStageSkipsDefendersTurnWhenNoDefender
    , testProperty "check stage skips scapegoat's turn when no seer"            prop_checkStageSkipsScapegoatsTurnWhenNoScapegoat
    , testProperty "check stage skips seer's turn when no seer"                 prop_checkStageSkipsSeersTurnWhenNoSeer
    , testProperty "check stage skips village's turn when allowed voters empty" prop_checkStageSkipsVillagesTurnWhenAllowedVotersEmpty
    , testProperty "check stage skips wild-child's turn when no wild-child"     prop_checkStageSkipsWildChildsTurnWhenNoWildChild
    , testProperty "check stage skips witch's turn when no witch"               prop_checkStageSkipsWitchsTurnWhenNoWitch
    , testProperty "check stage skips wolf-hound's turn when no wolf-hound"     prop_checkStageSkipsWolfHoundsTurnWhenNoWolfHound
    , testProperty "check stage does nothing when game over"                    prop_checkStageDoesNothingWhenGameOver

    , testProperty "check defender's turn advances to werewolves' turn"     prop_checkDefendersTurnAdvancesToWerewolvesTurn
    , testProperty "check defender's turn advances when no defender"        prop_checkDefendersTurnAdvancesWhenNoDefender
    , testProperty "check defender's turn does nothing unless protected"    prop_checkDefendersTurnDoesNothingUnlessProtected

    , testProperty "check scapegoat's turn advances to wolf-hound's turn"       prop_checkScapegoatsTurnAdvancesToWolfHoundsTurn
    , testProperty "check scapegoat's turn does nothing while scapegoat blamed" prop_checkScapegoatsTurnDoesNothingWhileScapegoatBlamed

    , testProperty "check seer's turn advances to wild-child's turn"    prop_checkSeersTurnAdvancesToWildChildsTurn
    , testProperty "check seer's turn advances when no seer"            prop_checkSeersTurnAdvancesWhenNoSeer
    , testProperty "check seer's turn resets sees"                      prop_checkSeersTurnResetsSee
    , testProperty "check seer's turn does nothing unless seen"         prop_checkSeersTurnDoesNothingUnlessSeen

    , testProperty "check sunrise increments round"         prop_checkSunriseIncrementsRound
    , testProperty "check sunrise sets angel's allegiance"  prop_checkSunriseSetsAngelsAllegiance

    , testProperty "check sunset sets wild-child's allegiance when role model dead" prop_checkSunsetSetsWildChildsAllegianceWhenRoleModelDead

    , testProperty "check villages' turn advances to scapegoat's turn"                      prop_checkVillagesTurnAdvancesToScapegoatsTurn
    , testProperty "check villages' turn lynches one player when consensus"                 prop_checkVillagesTurnLynchesOnePlayerWhenConsensus
    , testProperty "check villages' turn lynches no one when target is village idiot"       prop_checkVillagesTurnLynchesNoOneWhenTargetIsVillageIdiot
    , testProperty "check villages' turn lynches no one when conflicted and no scapegoats"  prop_checkVillagesTurnLynchesNoOneWhenConflictedAndNoScapegoats
    , testProperty "check villages' turn lynches scapegoat when conflicted"                 prop_checkVillagesTurnLynchesScapegoatWhenConflicted
    , testProperty "check villages' turn resets votes"                                      prop_checkVillagesTurnResetsVotes
    , testProperty "check villages' turn sets allowed voters"                               prop_checkVillagesTurnSetsAllowedVoters
    , testProperty "check villages' turn does nothing unless all voted"                     prop_checkVillagesTurnDoesNothingUnlessAllVoted

    , testProperty "check werewolves' turn advances to witch's turn"                    prop_checkWerewolvesTurnAdvancesToWitchsTurn
    , testProperty "check werewolves' turn skips witch's turn when healed and poisoned" prop_checkWerewolvesTurnSkipsWitchsTurnWhenHealedAndPoisoned
    , testProperty "check werewolves' turn kills one player when consensus"             prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus
    , testProperty "check werewolves' turn kills no one when conflicted"                prop_checkWerewolvesTurnKillsNoOneWhenConflicted
    , testProperty "check werewolves' turn kills no one when target defended"           prop_checkWerewolvesTurnKillsNoOneWhenTargetDefended
    , testProperty "check werewolves' turn resets protect"                              prop_checkWerewolvesTurnResetsProtect
    , testProperty "check werewolves' turn resets votes"                                prop_checkWerewolvesTurnResetsVotes
    , testProperty "check werewolves' turn does nothing unless all voted"               prop_checkWerewolvesTurnDoesNothingUnlessAllVoted

    , testProperty "check wild-child's turn advances to defender's turn"    prop_checkWildChildsTurnAdvancesToDefendersTurn
    , testProperty "check wild-child's turn advances when no wild-child"    prop_checkWildChildsTurnAdvancesWhenNoWildChild
    , testProperty "check wild-child's turn does nothing unless chosen"     prop_checkWildChildsTurnDoesNothingUnlessRoleModelChosen

    , testProperty "check witch's turn advances to villages' turn"              prop_checkWitchsTurnAdvancesToVillagesTurn
    , testProperty "check witch's turn advances when no witch"                  prop_checkWitchsTurnAdvancesWhenNoWitch
    , testProperty "check witch's turn heals devouree when healed"              prop_checkWitchsTurnHealsDevoureeWhenHealed
    , testProperty "check witch's turn kills one player when poisoned"          prop_checkWitchsTurnKillsOnePlayerWhenPoisoned
    , testProperty "check witch's turn does nothing when passed"                prop_checkWitchsTurnDoesNothingWhenPassed
    , testProperty "check witch's turn does nothing unless actioned or passed"  prop_checkWitchsTurnDoesNothingUnlessActionedOrPassed
    , testProperty "check witch's turn resets heal"                             prop_checkWitchsTurnResetsHeal
    , testProperty "check witch's turn resets poison"                           prop_checkWitchsTurnResetsPoison
    , testProperty "check witch's turn clears passes"                           prop_checkWitchsTurnClearsPasses

    , testProperty "check wolf-hound's turn advances to seer's turn"                prop_checkWolfHoundsTurnAdvancesToSeersTurn
    , testProperty "check wolf-hound's turn advances when no wolf-hound"            prop_checkWolfHoundsTurnAdvancesWhenNoWolfHound
    , testProperty "check wolf-hound's turn sets wolf-hound's allegiance"           prop_checkWolfHoundsTurnSetsWolfHoundsAllegiance
    , testProperty "check wolf-hound's turn does nothing unless allegiance chosen"  prop_checkWolfHoundsTurnDoesNothingUnlessAllegianceChosen

    , testProperty "check game over advances stage when one allegiance alive"                   prop_checkGameOverAdvancesStageWhenOneAllegianceAlive
    -- TODO (hjw): pending
    --, testProperty "check game over does nothing when at least two allegiances alive"   prop_checkGameOverDoesNothingWhenAtLeastTwoAllegiancesAlive
    , testProperty "check game over advances stage when after first round and angel dead"       prop_checkGameOverAdvancesStageWhenAfterFirstRoundAndAngelDead
    , testProperty "check game over does nothing when angel dead but aligned with villagers"    prop_checkGameOverDoesNothingWhenAngelDeadButAlignedWithVillagers

    , testProperty "start game uses given players"                              prop_startGameUsesGivenPlayers
    , testProperty "start game errors unless unique player names"               prop_startGameErrorsUnlessUniquePlayerNames
    , testProperty "start game errors when less than 7 players"                 prop_startGameErrorsWhenLessThan7Players
    , testProperty "start game errors when more than 24 players"                prop_startGameErrorsWhenMoreThan24Players
    , testProperty "start game errors when more than 1 of a restricted role"    prop_startGameErrorsWhenMoreThan1OfARestrictedRole
    ]

prop_checkStageSkipsDefendersTurnWhenNoDefender :: GameWithRoleModel -> Bool
prop_checkStageSkipsDefendersTurnWhenNoDefender (GameWithRoleModel game) =
    hasn't (stage . _DefendersTurn) game'
    where
        defendersName   = game ^?! players . defenders . name
        game'           = run_ (apply (quitCommand defendersName) >> checkStage) game

prop_checkStageSkipsScapegoatsTurnWhenNoScapegoat :: GameWithLynchVotes -> Bool
prop_checkStageSkipsScapegoatsTurnWhenNoScapegoat (GameWithLynchVotes game) =
    hasn't (stage . _ScapegoatsTurn) game'
    where
        scapegoatsName  = game ^?! players . scapegoats . name
        game'           = run_ (apply (quitCommand scapegoatsName) >> checkStage) game

prop_checkStageSkipsSeersTurnWhenNoSeer :: GameWithLynchVotes -> Bool
prop_checkStageSkipsSeersTurnWhenNoSeer (GameWithLynchVotes game) =
    hasn't (stage . _SeersTurn) game'
    where
        seersName   = game ^?! players . seers . name
        game'       = run_ (apply (quitCommand seersName) >> checkStage) game

prop_checkStageSkipsVillagesTurnWhenAllowedVotersEmpty :: GameAtWitchsTurn -> Property
prop_checkStageSkipsVillagesTurnWhenAllowedVotersEmpty (GameAtWitchsTurn game) =
    forAll (arbitraryPassCommand game') $ \(Blind passCommand) -> do
        hasn't (stage . _VillagesTurn) (run_ (apply passCommand >> checkStage) game')
    where
        game' = game & allowedVoters .~ []

prop_checkStageSkipsWildChildsTurnWhenNoWildChild :: GameWithSee -> Bool
prop_checkStageSkipsWildChildsTurnWhenNoWildChild (GameWithSee game) =
    hasn't (stage . _WildChildsTurn) game'
    where
        wildChildsName  = game ^?! players . wildChildren . name
        game'           = run_ (apply (quitCommand wildChildsName) >> checkStage) game

prop_checkStageSkipsWitchsTurnWhenNoWitch :: GameWithDevourVotes -> Property
prop_checkStageSkipsWitchsTurnWhenNoWitch (GameWithDevourVotes game) =
    null (run_ checkStage game' ^.. players . angels . dead)
    ==> hasn't (stage . _WitchsTurn) game'
    where
        witchsName  = game ^?! players . witches . name
        game'       = run_ (apply (quitCommand witchsName) >> checkStage) game

prop_checkStageSkipsWolfHoundsTurnWhenNoWolfHound :: GameWithProtect -> Bool
prop_checkStageSkipsWolfHoundsTurnWhenNoWolfHound (GameWithProtect game) =
    hasn't (stage . _WolfHoundsTurn) game'
    where
        wolfHoundsName  = game ^?! players . wolfHounds . name
        game'           = run_ (apply (quitCommand wolfHoundsName) >> checkStage) game

prop_checkStageDoesNothingWhenGameOver :: GameAtGameOver -> Property
prop_checkStageDoesNothingWhenGameOver (GameAtGameOver game) =
    run_ checkStage game === game

prop_checkDefendersTurnAdvancesToWerewolvesTurn :: GameWithProtect -> Bool
prop_checkDefendersTurnAdvancesToWerewolvesTurn (GameWithProtect game) =
    has (stage . _WerewolvesTurn) (run_ checkStage game)

prop_checkDefendersTurnAdvancesWhenNoDefender :: GameAtDefendersTurn -> Bool
prop_checkDefendersTurnAdvancesWhenNoDefender (GameAtDefendersTurn game) = do
    let defender    = game ^?! players . defenders
    let command     = quitCommand $ defender ^. name

    hasn't (stage . _DefendersTurn) (run_ (apply command >> checkStage) game)

prop_checkDefendersTurnDoesNothingUnlessProtected :: GameAtDefendersTurn -> Bool
prop_checkDefendersTurnDoesNothingUnlessProtected (GameAtDefendersTurn game) =
    has (stage . _DefendersTurn) (run_ checkStage game)

prop_checkScapegoatsTurnAdvancesToWolfHoundsTurn :: GameWithAllowedVoters -> Bool
prop_checkScapegoatsTurnAdvancesToWolfHoundsTurn (GameWithAllowedVoters game) =
    has (stage . _WolfHoundsTurn) (run_ checkStage game)

prop_checkScapegoatsTurnDoesNothingWhileScapegoatBlamed :: GameAtScapegoatsTurn -> Bool
prop_checkScapegoatsTurnDoesNothingWhileScapegoatBlamed (GameAtScapegoatsTurn game) =
    has (stage . _ScapegoatsTurn) (run_ checkStage game)

prop_checkSeersTurnAdvancesToWildChildsTurn :: GameWithSee -> Bool
prop_checkSeersTurnAdvancesToWildChildsTurn (GameWithSee game) =
    has (stage . _WildChildsTurn) (run_ checkStage game)

prop_checkSeersTurnAdvancesWhenNoSeer :: GameAtSeersTurn -> Bool
prop_checkSeersTurnAdvancesWhenNoSeer (GameAtSeersTurn game) = do
    let seer    = game ^?! players . seers
    let command = quitCommand $ seer ^. name

    hasn't (stage . _SeersTurn) (run_ (apply command >> checkStage) game)

prop_checkSeersTurnResetsSee :: GameWithSee -> Bool
prop_checkSeersTurnResetsSee (GameWithSee game) =
    isNothing $ run_ checkStage game ^. see

prop_checkSeersTurnDoesNothingUnlessSeen :: GameAtSeersTurn -> Bool
prop_checkSeersTurnDoesNothingUnlessSeen (GameAtSeersTurn game) =
    has (stage . _SeersTurn) (run_ checkStage game)

prop_checkSunriseIncrementsRound :: GameAtSunrise -> Property
prop_checkSunriseIncrementsRound (GameAtSunrise game) =
    run_ checkStage game ^. round === game ^. round + 1

prop_checkSunriseSetsAngelsAllegiance :: GameAtSunrise -> Bool
prop_checkSunriseSetsAngelsAllegiance (GameAtSunrise game) = do
    let game' = run_ checkStage game

    is villager $ game' ^?! players . angels

prop_checkSunsetSetsWildChildsAllegianceWhenRoleModelDead :: GameWithRoleModelAtVillagesTurn -> Property
prop_checkSunsetSetsWildChildsAllegianceWhenRoleModelDead (GameWithRoleModelAtVillagesTurn game) = do
    let game' = foldr (\player -> run_ (apply $ voteLynchCommand (player ^. name) (roleModel' ^. name))) game (game ^. players)

    isn't angel roleModel' && isn't villageIdiot roleModel'
        ==> is werewolf $ run_ checkStage game' ^?! players . wildChildren
    where
        roleModel' = game ^?! players . traverse . filteredBy name (fromJust $ game ^. roleModel)

prop_checkVillagesTurnAdvancesToScapegoatsTurn :: GameWithScapegoatBlamed -> Bool
prop_checkVillagesTurnAdvancesToScapegoatsTurn (GameWithScapegoatBlamed game) =
    has (stage . _ScapegoatsTurn) (run_ checkStage game)

prop_checkVillagesTurnLynchesOnePlayerWhenConsensus :: GameWithLynchVotes -> Property
prop_checkVillagesTurnLynchesOnePlayerWhenConsensus (GameWithLynchVotes game) =
    length (getVoteResult game) == 1
    && isn't villageIdiot target
    ==> length (run_ checkStage game ^.. players . traverse . dead) == 1
    where
        target = head $ getVoteResult game

prop_checkVillagesTurnLynchesNoOneWhenTargetIsVillageIdiot :: GameAtVillagesTurn -> Bool
prop_checkVillagesTurnLynchesNoOneWhenTargetIsVillageIdiot (GameAtVillagesTurn game) = do
    let game' = foldr (\player -> run_ (apply $ voteLynchCommand (player ^. name) (villageIdiot ^. name))) game (game ^. players)

    none (is dead) (run_ checkStage game' ^. players)
    where
        villageIdiot = game ^?! players . villageIdiots

-- TODO (hjw): tidy this test
prop_checkVillagesTurnLynchesNoOneWhenConflictedAndNoScapegoats :: Game -> Property
prop_checkVillagesTurnLynchesNoOneWhenConflictedAndNoScapegoats game =
    forAll (runArbitraryCommands n game') $ \game'' ->
    length (getVoteResult game'') > 1
    ==> run_ checkStage game'' ^. players == game' ^. players
    where
        scapegoatsName  = game ^?! players . scapegoats . name
        game'           = killPlayer scapegoatsName game & stage .~ VillagesTurn
        n               = length $ game' ^. players

prop_checkVillagesTurnLynchesScapegoatWhenConflicted :: GameAtScapegoatsTurn -> Bool
prop_checkVillagesTurnLynchesScapegoatWhenConflicted (GameAtScapegoatsTurn game) =
    is dead $ run_ checkStage game ^?! players . scapegoats

prop_checkVillagesTurnResetsVotes :: GameWithLynchVotes -> Bool
prop_checkVillagesTurnResetsVotes (GameWithLynchVotes game) =
    Map.null $ run_ checkStage game ^. votes

prop_checkVillagesTurnSetsAllowedVoters :: GameWithLynchVotes -> Property
prop_checkVillagesTurnSetsAllowedVoters (GameWithLynchVotes game) =
    game' ^. allowedVoters === expectedAllowedVoters ^.. names
    where
        game' = run_ checkStage game
        expectedAllowedVoters
            | game' ^. villageIdiotRevealed = filter (isn't villageIdiot) $ game' ^. players
            | otherwise                     = game' ^.. players . traverse . alive

prop_checkVillagesTurnDoesNothingUnlessAllVoted :: GameAtVillagesTurn -> Property
prop_checkVillagesTurnDoesNothingUnlessAllVoted (GameAtVillagesTurn game) =
    forAll (runArbitraryCommands n game) $ \game' ->
    has (stage . _VillagesTurn) (run_ checkStage game')
    where
        n = length (game ^. players) - 1

prop_checkWerewolvesTurnAdvancesToWitchsTurn :: GameWithDevourVotes -> Property
prop_checkWerewolvesTurnAdvancesToWitchsTurn (GameWithDevourVotes game) =
    length (getVoteResult game) == 1
    ==> has (stage . _WitchsTurn) (run_ checkStage game)

prop_checkWerewolvesTurnSkipsWitchsTurnWhenHealedAndPoisoned :: GameWithDevourVotes -> Bool
prop_checkWerewolvesTurnSkipsWitchsTurnWhenHealedAndPoisoned (GameWithDevourVotes game) =
    hasn't (stage . _WitchsTurn) (run_ checkStage game')
    where
        game' = game & healUsed .~ True & poisonUsed .~ True

prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus :: GameWithDevourVotes -> Property
prop_checkWerewolvesTurnKillsOnePlayerWhenConsensus (GameWithDevourVotes game) =
    length (getVoteResult game) == 1
    ==> has (events . traverse . _DevourEvent) (run_ checkStage game)

prop_checkWerewolvesTurnKillsNoOneWhenConflicted :: GameWithDevourVotes -> Property
prop_checkWerewolvesTurnKillsNoOneWhenConflicted (GameWithDevourVotes game) =
    length (getVoteResult game) > 1
    ==> hasn't (events . traverse . _DevourEvent) (run_ checkStage game)

prop_checkWerewolvesTurnKillsNoOneWhenTargetDefended :: GameWithProtectAndDevourVotes -> Property
prop_checkWerewolvesTurnKillsNoOneWhenTargetDefended (GameWithProtectAndDevourVotes game) =
    length (getVoteResult game) == 1
    ==> hasn't (events . traverse . _DevourEvent) (run_ checkStage game')
    where
        target  = head $ getVoteResult game
        game'   = game & protect .~ Just (target ^. name)

prop_checkWerewolvesTurnResetsProtect :: GameWithProtectAndDevourVotes -> Bool
prop_checkWerewolvesTurnResetsProtect (GameWithProtectAndDevourVotes game) =
    isNothing $ run_ checkStage game ^. protect

prop_checkWerewolvesTurnResetsVotes :: GameWithDevourVotes -> Bool
prop_checkWerewolvesTurnResetsVotes (GameWithDevourVotes game) =
    Map.null $ run_ checkStage game ^. votes

prop_checkWerewolvesTurnDoesNothingUnlessAllVoted :: GameAtWerewolvesTurn -> Property
prop_checkWerewolvesTurnDoesNothingUnlessAllVoted (GameAtWerewolvesTurn game) =
    forAll (runArbitraryCommands n game) $ \game' ->
    has (stage . _WerewolvesTurn) (run_ checkStage game')
    where
        n = length (game ^.. players . werewolves) - 1

prop_checkWildChildsTurnAdvancesToDefendersTurn :: GameAtWildChildsTurn -> Property
prop_checkWildChildsTurnAdvancesToDefendersTurn (GameAtWildChildsTurn game) =
    forAll (arbitraryChoosePlayerCommand game) $ \(Blind command) ->
    has (stage . _DefendersTurn) (run_ (apply command >> checkStage) game)

prop_checkWildChildsTurnAdvancesWhenNoWildChild :: GameAtWildChildsTurn -> Bool
prop_checkWildChildsTurnAdvancesWhenNoWildChild (GameAtWildChildsTurn game) = do
    let wildChild   = game ^?! players . wildChildren
    let command     = quitCommand $ wildChild ^. name

    hasn't (stage . _WildChildsTurn) (run_ (apply command >> checkStage) game)

prop_checkWildChildsTurnDoesNothingUnlessRoleModelChosen :: GameAtWildChildsTurn -> Bool
prop_checkWildChildsTurnDoesNothingUnlessRoleModelChosen (GameAtWildChildsTurn game) =
    has (stage . _WildChildsTurn) (run_ checkStage game)

prop_checkWitchsTurnAdvancesToVillagesTurn :: GameAtWitchsTurn -> Property
prop_checkWitchsTurnAdvancesToVillagesTurn (GameAtWitchsTurn game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) ->
    has (stage . _VillagesTurn) (run_ (apply command >> checkStage) game)

prop_checkWitchsTurnAdvancesWhenNoWitch :: GameAtWitchsTurn -> Bool
prop_checkWitchsTurnAdvancesWhenNoWitch (GameAtWitchsTurn game) = do
    let witch   = game ^?! players . witches
    let command = quitCommand $ witch ^. name

    hasn't (stage . _WitchsTurn) (run_ (apply command >> checkStage) game)

prop_checkWitchsTurnHealsDevoureeWhenHealed :: GameWithHeal -> Property
prop_checkWitchsTurnHealsDevoureeWhenHealed (GameWithHeal game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) ->
    none (is dead) (run_ (apply command >> checkStage) game ^. players)

prop_checkWitchsTurnKillsOnePlayerWhenPoisoned :: GameWithPoison -> Property
prop_checkWitchsTurnKillsOnePlayerWhenPoisoned (GameWithPoison game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) ->
    length (run_ (apply command >> checkStage) game ^.. players . traverse . dead) == 1

prop_checkWitchsTurnDoesNothingWhenPassed :: GameAtWitchsTurn -> Property
prop_checkWitchsTurnDoesNothingWhenPassed (GameAtWitchsTurn game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) ->
    none (is dead) $ run_ (apply command >> checkStage) game ^. players

prop_checkWitchsTurnDoesNothingUnlessActionedOrPassed :: GameAtWitchsTurn -> Bool
prop_checkWitchsTurnDoesNothingUnlessActionedOrPassed (GameAtWitchsTurn game) =
    has (stage . _WitchsTurn) (run_ checkStage game)

prop_checkWitchsTurnResetsHeal :: GameWithHeal -> Property
prop_checkWitchsTurnResetsHeal (GameWithHeal game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) ->
    not $ run_ (apply command >> checkStage) game ^. heal

prop_checkWitchsTurnResetsPoison :: GameWithPoison -> Property
prop_checkWitchsTurnResetsPoison (GameWithPoison game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) ->
    isNothing $ run_ (apply command >> checkStage) game ^. poison

prop_checkWitchsTurnClearsPasses :: GameAtWitchsTurn -> Property
prop_checkWitchsTurnClearsPasses (GameAtWitchsTurn game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) ->
    null $ run_ (apply command >> checkStage) game ^. passes

prop_checkWolfHoundsTurnAdvancesToSeersTurn :: GameAtWolfHoundsTurn -> Property
prop_checkWolfHoundsTurnAdvancesToSeersTurn (GameAtWolfHoundsTurn game) =
    forAll (arbitraryChooseAllegianceCommand game) $ \(Blind command) ->
    has (stage . _SeersTurn) (run_ (apply command >> checkStage) game)

prop_checkWolfHoundsTurnAdvancesWhenNoWolfHound :: GameAtWolfHoundsTurn -> Bool
prop_checkWolfHoundsTurnAdvancesWhenNoWolfHound (GameAtWolfHoundsTurn game) = do
    let wolfHound   = game ^?! players . wolfHounds
    let command     = quitCommand $ wolfHound ^. name

    hasn't (stage . _WolfHoundsTurn) (run_ (apply command >> checkStage) game)

prop_checkWolfHoundsTurnSetsWolfHoundsAllegiance :: GameWithAllegianceChosen -> Property
prop_checkWolfHoundsTurnSetsWolfHoundsAllegiance (GameWithAllegianceChosen game) =
    game' ^?! players . wolfHounds . role . allegiance === fromJust (game' ^. allegianceChosen)
    where
        game' = run_ checkStage game

prop_checkWolfHoundsTurnDoesNothingUnlessAllegianceChosen :: GameAtWolfHoundsTurn -> Bool
prop_checkWolfHoundsTurnDoesNothingUnlessAllegianceChosen (GameAtWolfHoundsTurn game) =
    has (stage . _WolfHoundsTurn) (run_ checkStage game)

prop_checkGameOverAdvancesStageWhenOneAllegianceAlive :: GameWithOneAllegianceAlive -> Property
prop_checkGameOverAdvancesStageWhenOneAllegianceAlive (GameWithOneAllegianceAlive game) =
    forAll (sublistOf $ game ^.. players . traverse . alive) $ \players' -> do
        let game' = foldr killPlayer game (players' ^.. names)

        has (stage . _GameOver) $ run_ checkGameOver game'

-- TODO (hjw): pending
--prop_checkGameOverDoesNothingWhenAtLeastTwoAllegiancesAlive :: GameWithDeadPlayers -> Property
--prop_checkGameOverDoesNothingWhenAtLeastTwoAllegiancesAlive (GameWithDeadPlayers game) =
--    length (nub . map (view $ role . allegiance) . filterAlive $ game ^. players) > 1
--    ==> not . is gameOver $ run_ checkGameOver game

prop_checkGameOverAdvancesStageWhenAfterFirstRoundAndAngelDead :: GameOnSecondRound -> Bool
prop_checkGameOverAdvancesStageWhenAfterFirstRoundAndAngelDead (GameOnSecondRound game) = do
    let angelsName  = game ^?! players . angels . name
    let game'       = killPlayer angelsName game

    has (stage . _GameOver) $ run_ checkGameOver game'

prop_checkGameOverDoesNothingWhenAngelDeadButAlignedWithVillagers :: GameOnSecondRound -> Bool
prop_checkGameOverDoesNothingWhenAngelDeadButAlignedWithVillagers (GameOnSecondRound game) = do
    let angelsName  = game ^?! players . angels . name
    let game'       = killPlayer angelsName (setPlayerAllegiance angelsName Villagers game)

    hasn't (stage . _GameOver) $ run_ checkGameOver game'

prop_startGameUsesGivenPlayers :: Property
prop_startGameUsesGivenPlayers =
    forAll arbitraryPlayerSet $ \players' ->
    (fst . fromRight . runExcept . runWriterT $ startGame "" players') ^. players == players'

prop_startGameErrorsUnlessUniquePlayerNames :: Game -> Property
prop_startGameErrorsUnlessUniquePlayerNames game =
    forAll (elements players') $ \player ->
    isLeft (runExcept . runWriterT $ startGame "" (player:players'))
    where
        players' = game ^. players

prop_startGameErrorsWhenLessThan7Players :: [Player] -> Property
prop_startGameErrorsWhenLessThan7Players players =
    length players < 7
    ==> isLeft . runExcept . runWriterT $ startGame "" players

prop_startGameErrorsWhenMoreThan24Players :: Property
prop_startGameErrorsWhenMoreThan24Players =
    forAll (resize 30 $ listOf arbitrary) $ \players ->
        length players > 24
        ==> isLeft . runExcept . runWriterT $ startGame "" players

prop_startGameErrorsWhenMoreThan1OfARestrictedRole :: [Player] -> Property
prop_startGameErrorsWhenMoreThan1OfARestrictedRole players =
    any (\role' -> length (players ^.. traverse . filteredBy role role') > 1) restrictedRoles
    ==> isLeft . runExcept . runWriterT $ startGame "" players
