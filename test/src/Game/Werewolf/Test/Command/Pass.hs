{-|
Module      : Game.Werewolf.Test.Command.Pass
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Command.Pass (
    -- * Tests
    allPassCommandTests,
) where

import Control.Lens hiding (elements, isn't)

import Game.Werewolf.Command
import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allPassCommandTests :: [TestTree]
allPassCommandTests =
    [ testProperty "pass devoted servant's turn command errors when game is over"               prop_passDevotedServantsTurnCommandErrorsWhenGameIsOver
    , testProperty "pass devoted servant's turn command errors when caller does not exist"      prop_passDevotedServantsTurnCommandErrorsWhenCallerDoesNotExist
    , testProperty "pass devoted servant's turn command errors when caller is dead"             prop_passDevotedServantsTurnCommandErrorsWhenCallerIsDead
    , testProperty "pass devoted servant's turn command errors when not devoted servant's turn" prop_passDevotedServantsTurnCommandErrorsWhenNotDevotedServantsTurn
    , testProperty "pass devoted servant's turn command updates passes"                         prop_passDevotedServantsTurnCommandUpdatesPasses

    , testProperty "pass witch's turn command errors when game is over"             prop_passWitchsTurnCommandErrorsWhenGameIsOver
    , testProperty "pass witch's turn command errors when caller does not exist"    prop_passWitchsTurnCommandErrorsWhenCallerDoesNotExist
    , testProperty "pass witch's turn command errors when caller is dead"           prop_passWitchsTurnCommandErrorsWhenCallerIsDead
    , testProperty "pass witch's turn command errors when not witch's turn"         prop_passWitchsTurnCommandErrorsWhenNotWitchsTurn
    , testProperty "pass witch's turn command updates passes"                       prop_passWitchsTurnCommandUpdatesPasses
    ]

prop_passDevotedServantsTurnCommandErrorsWhenGameIsOver :: Game -> Bool
prop_passDevotedServantsTurnCommandErrorsWhenGameIsOver = undefined

prop_passDevotedServantsTurnCommandErrorsWhenCallerDoesNotExist :: Game -> Bool
prop_passDevotedServantsTurnCommandErrorsWhenCallerDoesNotExist = undefined

prop_passDevotedServantsTurnCommandErrorsWhenCallerIsDead :: Game -> Bool
prop_passDevotedServantsTurnCommandErrorsWhenCallerIsDead = undefined

prop_passDevotedServantsTurnCommandErrorsWhenNotDevotedServantsTurn :: Game -> Bool
prop_passDevotedServantsTurnCommandErrorsWhenNotDevotedServantsTurn = undefined

prop_passDevotedServantsTurnCommandUpdatesPasses :: Game -> Bool
prop_passDevotedServantsTurnCommandUpdatesPasses = undefined

prop_passWitchsTurnCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_passWitchsTurnCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryPassWitchsTurnCommand game) $ verbose_runCommandErrors game . getBlind

prop_passWitchsTurnCommandErrorsWhenCallerDoesNotExist :: GameAtWitchsTurn -> Player -> Property
prop_passWitchsTurnCommandErrorsWhenCallerDoesNotExist (GameAtWitchsTurn game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> verbose_runCommandErrors game (passWitchsTurnCommand (caller ^. name))

prop_passWitchsTurnCommandErrorsWhenCallerIsDead :: GameAtWitchsTurn -> Property
prop_passWitchsTurnCommandErrorsWhenCallerIsDead (GameAtWitchsTurn game) =
    forAll (arbitraryPlayer game) $ \caller -> do
        let game'   = killPlayer (caller ^. name) game
        let command = passWitchsTurnCommand (caller ^. name)

        verbose_runCommandErrors game' command

prop_passWitchsTurnCommandErrorsWhenNotWitchsTurn :: Game -> Property
prop_passWitchsTurnCommandErrorsWhenNotWitchsTurn game =
    hasn't (stage . _WitchsTurn) game
    ==> forAll (arbitraryPassWitchsTurnCommand game) $ verbose_runCommandErrors game . getBlind

prop_passWitchsTurnCommandUpdatesPasses :: GameAtWitchsTurn -> Property
prop_passWitchsTurnCommandUpdatesPasses (GameAtWitchsTurn game) =
    forAll (arbitraryPassWitchsTurnCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command) game

        length (game' ^. passes) == 1
