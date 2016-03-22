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

import Game.Werewolf
import Game.Werewolf.Command.DevotedServant as DevotedServant
import Game.Werewolf.Command.Witch          as Witch
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allPassCommandTests :: [TestTree]
allPassCommandTests =
    [ testProperty "devoted servant pass command errors when game is over"                  prop_devotedServantPassCommandErrorsWhenGameIsOver
    , testProperty "devoted servant pass command errors when caller does not exist"         prop_devotedServantPassCommandErrorsWhenCallerDoesNotExist
    , testProperty "devoted servant pass command errors when caller is dead"                prop_devotedServantPassCommandErrorsWhenCallerIsDead
    , testProperty "devoted servant pass command errors when not devoted servant's turn"    prop_devotedServantPassCommandErrorsWhenNotDevotedServantsTurn
    , testProperty "devoted servant pass command updates passes"                            prop_devotedServantPassCommandUpdatesPasses

    , testProperty "witch pass command errors when game is over"            prop_witchPassCommandErrorsWhenGameIsOver
    , testProperty "witch pass command errors when caller does not exist"   prop_witchPassCommandErrorsWhenCallerDoesNotExist
    , testProperty "witch pass command errors when caller is dead"          prop_witchPassCommandErrorsWhenCallerIsDead
    , testProperty "witch pass command errors when not witch's turn"        prop_witchPassCommandErrorsWhenNotWitchsTurn
    , testProperty "witch pass command updates passes"                      prop_witchPassCommandUpdatesPasses
    ]

prop_devotedServantPassCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_devotedServantPassCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryDevotedServantPassCommand game) $ verbose_runCommandErrors game . getBlind

prop_devotedServantPassCommandErrorsWhenCallerDoesNotExist :: GameAtDevotedServantsTurn -> Player -> Property
prop_devotedServantPassCommandErrorsWhenCallerDoesNotExist (GameAtDevotedServantsTurn game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> verbose_runCommandErrors game (DevotedServant.passCommand (caller ^. name))

prop_devotedServantPassCommandErrorsWhenCallerIsDead :: GameAtDevotedServantsTurn -> Property
prop_devotedServantPassCommandErrorsWhenCallerIsDead (GameAtDevotedServantsTurn game) = do
    let devotedServantsName = game ^?! players . devotedServants . name
    let game'               = killPlayer devotedServantsName game
    let command             = DevotedServant.passCommand devotedServantsName

    verbose_runCommandErrors game' command

prop_devotedServantPassCommandErrorsWhenNotDevotedServantsTurn :: Game -> Property
prop_devotedServantPassCommandErrorsWhenNotDevotedServantsTurn game =
    hasn't (stage . _DevotedServantsTurn) game
    ==> forAll (arbitraryDevotedServantPassCommand game) $ verbose_runCommandErrors game . getBlind

prop_devotedServantPassCommandUpdatesPasses :: GameAtDevotedServantsTurn -> Property
prop_devotedServantPassCommandUpdatesPasses (GameAtDevotedServantsTurn game) =
    forAll (arbitraryDevotedServantPassCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command) game

        length (game' ^. passes) == 1

prop_witchPassCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_witchPassCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryWitchPassCommand game) $ verbose_runCommandErrors game . getBlind

prop_witchPassCommandErrorsWhenCallerDoesNotExist :: GameAtWitchsTurn -> Player -> Property
prop_witchPassCommandErrorsWhenCallerDoesNotExist (GameAtWitchsTurn game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> verbose_runCommandErrors game (Witch.passCommand (caller ^. name))

prop_witchPassCommandErrorsWhenCallerIsDead :: GameAtWitchsTurn -> Property
prop_witchPassCommandErrorsWhenCallerIsDead (GameAtWitchsTurn game) = do
    let witchsName  = game ^?! players . witches . name
    let game'       = killPlayer witchsName game
    let command     = Witch.passCommand witchsName

    verbose_runCommandErrors game' command

prop_witchPassCommandErrorsWhenNotWitchsTurn :: Game -> Property
prop_witchPassCommandErrorsWhenNotWitchsTurn game =
    hasn't (stage . _WitchsTurn) game
    ==> forAll (arbitraryWitchPassCommand game) $ verbose_runCommandErrors game . getBlind

prop_witchPassCommandUpdatesPasses :: GameAtWitchsTurn -> Property
prop_witchPassCommandUpdatesPasses (GameAtWitchsTurn game) =
    forAll (arbitraryWitchPassCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command) game

        length (game' ^. passes) == 1
