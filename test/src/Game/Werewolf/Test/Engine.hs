{-|
Module      : Game.Werewolf.Test.Engine
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Engine (
    -- * startGame
    prop_startGameStartsWithSeersTurn, prop_startGameUsesGivenPlayers,
    prop_startGameErrorsUnlessUniquePlayerNames, prop_startGameErrorsWhenLessThan7Players,
    prop_startGameErrorsWhenMoreThan24Players,

    -- * createPlayers
    prop_createPlayersUsesGivenPlayerNames, prop_createPlayersCreatesAlivePlayers,

    -- * randomiseRoles
    prop_randomiseRolesReturnsNRoles,
) where

import Control.Lens         hiding (elements)
import Control.Monad.Except

import Data.Either.Extra
import Data.Text         (Text)

import Game.Werewolf.Engine         hiding (doesPlayerExist, isSeersTurn)
import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Test.Arbitrary ()

import Test.QuickCheck
import Test.QuickCheck.Monadic

-- TODO (hjw)
--prop_checkGameOverAdvancesTurn

-- TODO (hjw)
--prop_checkGameOverDoesNothingWhenAtLeast2PlayersAlive

prop_startGameStartsWithSeersTurn :: [Player] -> Property
prop_startGameStartsWithSeersTurn players = and [
    isRight . runExcept $ startGame "" players
    ] ==> isSeersTurn (fromRight . runExcept $ startGame "" players)

prop_startGameUsesGivenPlayers :: [Player] -> Property
prop_startGameUsesGivenPlayers players_ = and [
    isRight . runExcept $ startGame "" players_
    ] ==> (fromRight . runExcept $ startGame "" players_) ^. players == players_

prop_startGameErrorsUnlessUniquePlayerNames :: [Player] -> Property
prop_startGameErrorsUnlessUniquePlayerNames players = and [
    isRight . runExcept $ startGame "" players
    ] ==> forAll (elements players) $ \player -> isLeft (runExcept $ startGame "" (player:players))

prop_startGameErrorsWhenLessThan7Players :: [Player] -> Property
prop_startGameErrorsWhenLessThan7Players players = and [
    length players < 7
    ] ==> isLeft (runExcept $ startGame "" players)

prop_startGameErrorsWhenMoreThan24Players :: Property
prop_startGameErrorsWhenMoreThan24Players = forAll (resize 30 $ listOf arbitrary) $ \players -> and [
    length players > 24
    ] ==> isLeft (runExcept $ startGame "" players)

prop_createPlayersUsesGivenPlayerNames :: [Text] -> Property
prop_createPlayersUsesGivenPlayerNames playerNames = monadicIO $ createPlayers playerNames >>= return . (playerNames ==) . map _name

prop_createPlayersCreatesAlivePlayers :: [Text] -> Property
prop_createPlayersCreatesAlivePlayers playerNames = monadicIO $ createPlayers playerNames >>= return . all ((==) Alive . _state)

prop_randomiseRolesReturnsNRoles :: Int -> Property
prop_randomiseRolesReturnsNRoles n = monadicIO $ randomiseRoles n >>= return . (==) n . length

-- TODO (hjw)
--prop_randomiseRolesProportionsRoles

-- TODO (hjw)
--prop_randomiseRolesHas1Seer :: Int -> Property
--prop_randomiseRolesHas1Seer n = monadicIO $ randomiseRoles n >>=
