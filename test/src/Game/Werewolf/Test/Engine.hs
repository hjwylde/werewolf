{-|
Module      : Game.Werewolf.Test.Engine
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Engine (
    -- * validateCommand
    prop_validateVoteCommandErrorsWhenGameIsOver,
    prop_validateVoteCommandErrorsWhenVoterDoesNotExist,
    prop_validateVoteCommandErrorsWhenTargetDoesNotExist,
    prop_validateVoteCommandErrorsWhenVoterIsDead, prop_validateVoteCommandErrorsWhenTargetIsDead,
    prop_validateVoteCommandErrorsWhenVoterHasVoted,
    prop_validateKillVoteCommandErrorsWhenVoterNotWerewolf,

    -- * startGame
    prop_startGameStartsWithWerewolvesTurn, prop_startGameErrorsUnlessUniquePlayerNames,
    prop_startGameErrorsWhenLessThan7Players, prop_startGameErrorsWhenMoreThan24Players,

    -- * createPlayers
    prop_createPlayersUsesGivenPlayerNames, prop_createPlayersCreatesAlivePlayers,

    -- * randomiseRoles
    prop_randomiseRolesReturnsNRoles,
) where

import Control.Lens         hiding (elements)
import Control.Monad.Except
import Control.Monad.State  hiding (state)
import Control.Monad.Writer

import           Data.Either.Extra
import qualified Data.Map          as Map
import           Data.Text         (Text)

import Game.Werewolf.Command
import Game.Werewolf.Engine         hiding (doesPlayerExist)
import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Response
import Game.Werewolf.Test.Arbitrary

import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_validateVoteCommandErrorsWhenGameIsOver :: Game -> Property
prop_validateVoteCommandErrorsWhenGameIsOver game = forAll (arbitraryCommand game) $ \command -> and [
    game ^. turn == NoOne
    ] ==> verbose_validateCommandErrors game command

prop_validateVoteCommandErrorsWhenVoterDoesNotExist :: Game -> Player -> Property
prop_validateVoteCommandErrorsWhenVoterDoesNotExist game voter = forAll (elements $ game ^. players) $ \target -> and [
    game ^. turn /= NoOne,
    not $ doesPlayerExist (voter ^. name) (game ^. players)
    ] ==> verbose_validateCommandErrors game (Vote { voter = voter, target = target })

prop_validateVoteCommandErrorsWhenTargetDoesNotExist :: Game -> Player -> Property
prop_validateVoteCommandErrorsWhenTargetDoesNotExist game target = forAll (elements $ game ^. players) $ \voter -> and [
    game ^. turn /= NoOne,
    not $ doesPlayerExist (target ^. name) (game ^. players)
    ] ==> verbose_validateCommandErrors game (Vote { voter = voter, target = target })

prop_validateVoteCommandErrorsWhenVoterIsDead :: Game -> Property
prop_validateVoteCommandErrorsWhenVoterIsDead game = forAll (arbitraryCommand game) $ \command@(Vote voter _) -> and [
    game ^. turn /= NoOne,
    isDead voter
    ] ==> verbose_validateCommandErrors game command

prop_validateVoteCommandErrorsWhenTargetIsDead :: Game -> Property
prop_validateVoteCommandErrorsWhenTargetIsDead game = forAll (arbitraryCommand game) $ \command@(Vote _ target) -> and [
    game ^. turn /= NoOne,
    isDead target
    ] ==> verbose_validateCommandErrors game command

prop_validateVoteCommandErrorsWhenVoterHasVoted :: Game -> Property
prop_validateVoteCommandErrorsWhenVoterHasVoted game = forAll (arbitraryCommand game) $ \command -> and [
    isRight $ runCommand command game,
    Map.size ((runCommand_ command game) ^. turn . votes) == 1
    ] ==> verbose_validateCommandErrors (runCommand_ command game) command

prop_validateKillVoteCommandErrorsWhenVoterNotWerewolf :: Game -> Property
prop_validateKillVoteCommandErrorsWhenVoterNotWerewolf game = forAll (arbitraryCommand game) $ \command@(Vote voter _) -> and [
    isWerewolvesTurn game,
    not $ isWerewolf voter
    ] ==> verbose_validateCommandErrors game command

verbose_validateCommandErrors :: Game -> Command -> Property
verbose_validateCommandErrors game command = whenFail (mapM_ putStrLn [show game, show command, show . fromRight $ runCommand command game]) (isLeft $ runCommand command game)

runCommand :: Command -> Game -> Either [Message] (Game, [Message])
runCommand command = runExcept . runWriterT . execStateT (validateCommand command >> applyCommand command >> checkGameOver)

runCommand_ :: Command -> Game -> Game
runCommand_ command = fst . fromRight . runCommand command

-- TODO (hjw)
--prop_applyVoteComandRecordsVote

-- TODO (hjw)
--prop_applyVoteCommandKillsTargetWhenEveryoneVoted

-- TODO (hjw)
--prop_applyVoteCommandKillsNoOneWhenEveryoneVotedUnlessUnanimous

-- TODO (hjw)
--prop_applyVoteCommandAdvancesTurnWhenEveryoneVoted

-- TODO (hjw)
--prop_checkGameOverAdvancesTurn

-- TODO (hjw)
--prop_checkGameOverDoesNothingWhenAtLeast2PlayersAlive

prop_startGameStartsWithWerewolvesTurn :: [Player] -> Property
prop_startGameStartsWithWerewolvesTurn players = and [
    isRight . runExcept $ startGame "" players
    ] ==> isWerewolvesTurn (fromRight . runExcept $ startGame "" players)

prop_startGameErrorsUnlessUniquePlayerNames :: [Player] -> Property
prop_startGameErrorsUnlessUniquePlayerNames players = and [
    isRight . runExcept $ startGame "" players
    ] ==> forAll (elements players) $ \player -> isLeft (runExcept $ startGame "" (player:players))

prop_startGameErrorsWhenLessThan7Players :: [Player] -> Property
prop_startGameErrorsWhenLessThan7Players players = and [
    length players < 7
    ] ==> isLeft (runExcept $ startGame "" players)

prop_startGameErrorsWhenMoreThan24Players :: Property
prop_startGameErrorsWhenMoreThan24Players = forAll (resize 50 $ listOf arbitrary) $ \players -> and [
    length players > 24
    ] ==> isLeft (runExcept $ startGame "" players)

prop_createPlayersUsesGivenPlayerNames :: [Text] -> Property
prop_createPlayersUsesGivenPlayerNames playerNames = monadicIO $ createPlayers playerNames >>= return . (playerNames ==) . map _name

prop_createPlayersCreatesAlivePlayers :: [Text] -> Property
prop_createPlayersCreatesAlivePlayers playerNames = monadicIO $ createPlayers playerNames >>= return . all ((==) Alive . _state)

prop_randomiseRolesReturnsNRoles :: Int -> Property
prop_randomiseRolesReturnsNRoles n = monadicIO $ randomiseRoles n >>= return . (==) n . length

--prop_randomiseRolesProportionsRoles
