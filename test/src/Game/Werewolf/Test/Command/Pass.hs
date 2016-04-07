{-|
Module      : Game.Werewolf.Test.Command.Pass
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Command.Pass (
    -- * Tests
    allPassCommandTests,
) where

import Control.Lens hiding (elements, isn't)

import Game.Werewolf
import Game.Werewolf.Command.Witch
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allPassCommandTests :: [TestTree]
allPassCommandTests =
    [ testProperty "witch pass command errors when game is over"            prop_witchPassCommandErrorsWhenGameIsOver
    , testProperty "witch pass command errors when caller does not exist"   prop_witchPassCommandErrorsWhenCallerDoesNotExist
    , testProperty "witch pass command errors when caller is dead"          prop_witchPassCommandErrorsWhenCallerIsDead
    , testProperty "witch pass command errors when not witch's turn"        prop_witchPassCommandErrorsWhenNotWitchsTurn
    , testProperty "witch pass command sets passed"                         prop_witchPassCommandSetsPassed
    ]

prop_witchPassCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_witchPassCommandErrorsWhenGameIsOver (GameAtGameOver game) =
    forAll (arbitraryPassCommand game) $ verbose_runCommandErrors game . getBlind

prop_witchPassCommandErrorsWhenCallerDoesNotExist :: GameAtWitchsTurn -> Player -> Property
prop_witchPassCommandErrorsWhenCallerDoesNotExist (GameAtWitchsTurn game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> verbose_runCommandErrors game (passCommand $ caller ^. name)

prop_witchPassCommandErrorsWhenCallerIsDead :: GameAtWitchsTurn -> Property
prop_witchPassCommandErrorsWhenCallerIsDead (GameAtWitchsTurn game) = do
    let witchsName  = game ^?! players . witches . name
    let game'       = killPlayer witchsName game
    let command     = passCommand witchsName

    verbose_runCommandErrors game' command

prop_witchPassCommandErrorsWhenNotWitchsTurn :: Game -> Property
prop_witchPassCommandErrorsWhenNotWitchsTurn game =
    hasn't (stage . _WitchsTurn) game
    ==> forAll (arbitraryPassCommand game) $ verbose_runCommandErrors game . getBlind

prop_witchPassCommandSetsPassed :: GameAtWitchsTurn -> Property
prop_witchPassCommandSetsPassed (GameAtWitchsTurn game) =
    forAll (arbitraryPassCommand game) $ \(Blind command) -> do
        let game' = run_ (apply command) game

        game' ^. passed
