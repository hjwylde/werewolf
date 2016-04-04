{-|
Module      : Game.Werewolf.Test.Command.Reveal
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Command.Reveal (
    -- * Tests
    allRevealCommandTests,
) where

import Control.Lens hiding (isn't)

import Data.Maybe

import Game.Werewolf
import Game.Werewolf.Command.DevotedServant
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allRevealCommandTests :: [TestTree]
allRevealCommandTests =
    [ testProperty "reveal command errors when game is over"                prop_revealCommandErrorsWhenGameIsOver
    , testProperty "reveal command errors when caller does not exist"       prop_revealCommandErrorsWhenCallerDoesNotExist
    , testProperty "reveal command errors when caller is dead"              prop_revealCommandErrorsWhenCallerIsDead
    , testProperty "reveal command errors when not devoted servant's turn"  prop_revealCommandErrorsWhenNotDevotedServantsTurn
    , testProperty "reveal command errors when caller not devoted servant"  prop_revealCommandErrorsWhenCallerNotDevotedServant
    , testProperty "reveal command sets caller's role"                      prop_revealCommandSetsCallersRole
    , testProperty "reveal command sets target's role"                      prop_revealCommandSetsTargetsRole
    , testProperty "reveal command resets role when jester"                 prop_revealCommandResetsRoleWhenJester
    , testProperty "reveal command resets role when orphan"                 prop_revealCommandResetsRoleWhenOrphan
    , testProperty "reveal command resets role when witch"                  prop_revealCommandResetsRoleWhenWitch
    , testProperty "reveal command resets role when wolf-hound"             prop_revealCommandResetsRoleWhenWolfHound
    ]

prop_revealCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_revealCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryRevealCommand game) $ verbose_runCommandErrors game . getBlind

prop_revealCommandErrorsWhenCallerDoesNotExist :: GameAtDevotedServantsTurn -> Player -> Property
prop_revealCommandErrorsWhenCallerDoesNotExist (GameAtDevotedServantsTurn game) caller = do
    let command = revealCommand $ caller ^. name

    not (doesPlayerExist (caller ^. name) game)
        ==> verbose_runCommandErrors game command

prop_revealCommandErrorsWhenCallerIsDead :: GameAtDevotedServantsTurn -> Property
prop_revealCommandErrorsWhenCallerIsDead (GameAtDevotedServantsTurn game) = do
    let devotedServantsName = game ^?! players . devotedServants . name
    let game'               = killPlayer devotedServantsName game
    let command             = revealCommand devotedServantsName

    verbose_runCommandErrors game' command

prop_revealCommandErrorsWhenNotDevotedServantsTurn :: Game -> Property
prop_revealCommandErrorsWhenNotDevotedServantsTurn game =
    hasn't (stage . _DevotedServantsTurn) game
    ==> forAll (arbitraryRevealCommand game) $ verbose_runCommandErrors game . getBlind

prop_revealCommandErrorsWhenCallerNotDevotedServant :: GameAtDevotedServantsTurn -> Property
prop_revealCommandErrorsWhenCallerNotDevotedServant (GameAtDevotedServantsTurn game) =
    forAll (suchThat (arbitraryPlayer game) (isn't devotedServant)) $ \caller -> do
        let command = revealCommand $ caller ^. name

        verbose_runCommandErrors game command

prop_revealCommandSetsCallersRole :: GameAtDevotedServantsTurn -> Property
prop_revealCommandSetsCallersRole (GameAtDevotedServantsTurn game) = do
    let devotedServantsName = game ^?! players . devotedServants . name
    let command             = revealCommand devotedServantsName
    let game'               = run_ (apply command) game

    game' ^?! players . traverse . filteredBy name devotedServantsName . role === target ^. role
    where
        target = head $ getVoteResult game

prop_revealCommandSetsTargetsRole :: GameAtDevotedServantsTurn -> Bool
prop_revealCommandSetsTargetsRole (GameAtDevotedServantsTurn game) = do
    let devotedServantsName = game ^?! players . devotedServants . name
    let command             = revealCommand devotedServantsName
    let game'               = run_ (apply command) game

    is devotedServant $ game' ^?! players . traverse . filteredBy name targetsName
    where
        targetsName = head (getVoteResult game) ^. name

prop_revealCommandResetsRoleWhenJester :: GameAtDevotedServantsTurn -> Bool
prop_revealCommandResetsRoleWhenJester (GameAtDevotedServantsTurn game) = do
    let devotedServantsName = game ^?! players . devotedServants . name
    let command             = revealCommand devotedServantsName
    let game''              = run_ (apply command) game'

    not $ game'' ^. jesterRevealed
    where
        targetsName = head (getVoteResult game) ^. name
        game'       = game & players . traverse . filteredBy name targetsName . role .~ jesterRole & jesterRevealed .~ True

prop_revealCommandResetsRoleWhenOrphan :: GameAtDevotedServantsTurn -> Bool
prop_revealCommandResetsRoleWhenOrphan (GameAtDevotedServantsTurn game) = do
    let devotedServantsName = game ^?! players . devotedServants . name
    let command             = revealCommand devotedServantsName
    let game''              = run_ (apply command) game'

    isNothing $ game'' ^. roleModel
    where
        targetsName = head (getVoteResult game) ^. name
        game'       = game & players . traverse . filteredBy name targetsName . role .~ orphanRole & roleModel .~ Just targetsName

prop_revealCommandResetsRoleWhenWitch :: GameAtDevotedServantsTurn -> Bool
prop_revealCommandResetsRoleWhenWitch (GameAtDevotedServantsTurn game) = do
    let devotedServantsName = game ^?! players . devotedServants . name
    let command             = revealCommand devotedServantsName
    let game''              = run_ (apply command) game'

    not (game'' ^. healUsed) && not (game'' ^. poisonUsed)
    where
        targetsName = head (getVoteResult game) ^. name
        game'       = game & players . traverse . filteredBy name targetsName . role .~ witchRole & healUsed .~ True & poisonUsed .~ True

prop_revealCommandResetsRoleWhenWolfHound :: GameAtDevotedServantsTurn -> Bool
prop_revealCommandResetsRoleWhenWolfHound (GameAtDevotedServantsTurn game) = do
    let devotedServantsName = game ^?! players . devotedServants . name
    let command             = revealCommand devotedServantsName
    let game''              = run_ (apply command) game'

    not $ game'' ^. allegianceChosen
    where
        targetsName = head (getVoteResult game) ^. name
        game'       = game & players . traverse . filteredBy name targetsName . role .~ wolfHoundRole & allegianceChosen .~ True
