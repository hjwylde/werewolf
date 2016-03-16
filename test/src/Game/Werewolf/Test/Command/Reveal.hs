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

import Game.Werewolf.Game
import Game.Werewolf.Test.Arbitrary ()

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
    , testProperty "reveal command resets role when village idiot"          prop_revealCommandResetsRoleWhenVillageIdiot
    , testProperty "reveal command resets role when wild-child"             prop_revealCommandResetsRoleWhenWildChild
    , testProperty "reveal command resets role when witch"                  prop_revealCommandResetsRoleWhenWitch
    , testProperty "reveal command resets role when wolf-hound"             prop_revealCommandResetsRoleWhenWolfHound
    ]

prop_revealCommandErrorsWhenGameIsOver :: Game -> Bool
prop_revealCommandErrorsWhenGameIsOver = undefined

prop_revealCommandErrorsWhenCallerDoesNotExist :: Game -> Bool
prop_revealCommandErrorsWhenCallerDoesNotExist = undefined

prop_revealCommandErrorsWhenCallerIsDead :: Game -> Bool
prop_revealCommandErrorsWhenCallerIsDead = undefined

prop_revealCommandErrorsWhenNotDevotedServantsTurn :: Game -> Bool
prop_revealCommandErrorsWhenNotDevotedServantsTurn = undefined

prop_revealCommandErrorsWhenCallerNotDevotedServant :: Game -> Bool
prop_revealCommandErrorsWhenCallerNotDevotedServant = undefined

prop_revealCommandSetsCallersRole :: Game -> Bool
prop_revealCommandSetsCallersRole = undefined

prop_revealCommandSetsTargetsRole :: Game -> Bool
prop_revealCommandSetsTargetsRole = undefined

prop_revealCommandResetsRoleWhenVillageIdiot :: Game -> Bool
prop_revealCommandResetsRoleWhenVillageIdiot = undefined

prop_revealCommandResetsRoleWhenWildChild :: Game -> Bool
prop_revealCommandResetsRoleWhenWildChild = undefined

prop_revealCommandResetsRoleWhenWitch :: Game -> Bool
prop_revealCommandResetsRoleWhenWitch = undefined

prop_revealCommandResetsRoleWhenWolfHound :: Game -> Bool
prop_revealCommandResetsRoleWhenWolfHound = undefined
