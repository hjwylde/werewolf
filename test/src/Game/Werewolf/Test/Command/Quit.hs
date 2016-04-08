{-|
Module      : Game.Werewolf.Test.Command.Quit
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Command.Quit (
    -- * Tests
    allQuitCommandTests,
) where

import Control.Lens

import Game.Werewolf
import Game.Werewolf.Command.Global
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allQuitCommandTests :: [TestTree]
allQuitCommandTests =
    [ testProperty "quit command errors when game is over"          prop_quitCommandErrorsWhenGameIsOver
    , testProperty "quit command errors when caller does not exist" prop_quitCommandErrorsWhenCallerDoesNotExist
    , testProperty "quit command errors when caller is dead"        prop_quitCommandErrorsWhenCallerIsDead
    ]

prop_quitCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_quitCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryQuitCommand game) $ verbose_runCommandErrors game . getBlind

prop_quitCommandErrorsWhenCallerDoesNotExist :: Game -> Player -> Property
prop_quitCommandErrorsWhenCallerDoesNotExist game caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> verbose_runCommandErrors game (quitCommand $ caller ^. name)

prop_quitCommandErrorsWhenCallerIsDead :: Game -> Property
prop_quitCommandErrorsWhenCallerIsDead game =
    forAll (arbitraryPlayer game) $ \caller -> do
        let game'   = killPlayer (caller ^. name) game
        let command = quitCommand $ caller ^. name

        verbose_runCommandErrors game' command
