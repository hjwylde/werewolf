{-|
Module      : Game.Werewolf.Test.Command.Heal
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Game.Werewolf.Test.Command.Heal (
    -- * Tests
    allHealCommandTests,
) where

import Control.Lens hiding (elements, isn't)

import Game.Werewolf
import Game.Werewolf.Command.Witch
import Game.Werewolf.Test.Arbitrary
import Game.Werewolf.Test.Util

import Test.Tasty
import Test.Tasty.QuickCheck

allHealCommandTests :: [TestTree]
allHealCommandTests =
    [ testProperty "heal command errors when game is over"          prop_healCommandErrorsWhenGameIsOver
    , testProperty "heal command errors when caller does not exist" prop_healCommandErrorsWhenCallerDoesNotExist
    , testProperty "heal command errors when caller is dead"        prop_healCommandErrorsWhenCallerIsDead
    , testProperty "heal command errors when no target is devoured" prop_healCommandErrorsWhenNoTargetIsDevoured
    , testProperty "heal command errors when not witch's turn"      prop_healCommandErrorsWhenNotWitchsTurn
    , testProperty "heal command errors when caller has healed"     prop_healCommandErrorsWhenCallerHasHealed
    , testProperty "heal command errors when caller not witch"      prop_healCommandErrorsWhenCallerNotWitch
    , testProperty "heal command sets heal"                         prop_healCommandSetsHeal
    , testProperty "heal command sets heal used"                    prop_healCommandSetsHealUsed
    ]

prop_healCommandErrorsWhenGameIsOver :: GameAtGameOver -> Property
prop_healCommandErrorsWhenGameIsOver (GameAtGameOver game) = do
    let witch   = game ^?! players . witches
    let command = healCommand $ witch ^. name

    verbose_runCommandErrors game command

prop_healCommandErrorsWhenCallerDoesNotExist :: GameWithDevourEvent -> Player -> Property
prop_healCommandErrorsWhenCallerDoesNotExist (GameWithDevourEvent game) caller =
    not (doesPlayerExist (caller ^. name) game)
    ==> verbose_runCommandErrors game (healCommand (caller ^. name))

prop_healCommandErrorsWhenCallerIsDead :: GameWithDevourEvent -> Property
prop_healCommandErrorsWhenCallerIsDead (GameWithDevourEvent game) =
    forAll (arbitraryPlayer game) $ \caller -> do
        let game'   = killPlayer (caller ^. name) game
        let command = healCommand (caller ^. name)

        verbose_runCommandErrors game' command

prop_healCommandErrorsWhenNoTargetIsDevoured :: GameAtWitchsTurn -> Property
prop_healCommandErrorsWhenNoTargetIsDevoured (GameAtWitchsTurn game) = do
    let witch   = game ^?! players . witches
    let command = healCommand $ witch ^. name

    verbose_runCommandErrors game command

prop_healCommandErrorsWhenNotWitchsTurn :: Game -> Property
prop_healCommandErrorsWhenNotWitchsTurn game = do
    let witch   = game ^?! players . witches
    let command = healCommand $ witch ^. name

    hasn't (stage . _WitchsTurn) game
        ==> verbose_runCommandErrors game command

prop_healCommandErrorsWhenCallerHasHealed :: GameWithHeal -> Property
prop_healCommandErrorsWhenCallerHasHealed (GameWithHeal game) = do
    let witch   = game ^?! players . witches
    let command = healCommand $ witch ^. name

    verbose_runCommandErrors game command

prop_healCommandErrorsWhenCallerNotWitch :: GameWithDevourEvent -> Property
prop_healCommandErrorsWhenCallerNotWitch (GameWithDevourEvent game) =
    forAll (suchThat (arbitraryPlayer game) (isn't witch)) $ \caller -> do
        let command = healCommand (caller ^. name)

        verbose_runCommandErrors game command

prop_healCommandSetsHeal :: GameWithDevourEvent -> Property
prop_healCommandSetsHeal (GameWithDevourEvent game) =
    forAll (arbitraryHealCommand game) $ \(Blind command) ->
        (run_ (apply command) game) ^. heal

prop_healCommandSetsHealUsed :: GameWithDevourEvent -> Property
prop_healCommandSetsHealUsed (GameWithDevourEvent game) =
    forAll (arbitraryHealCommand game) $ \(Blind command) ->
        (run_ (apply command) game) ^. healUsed
