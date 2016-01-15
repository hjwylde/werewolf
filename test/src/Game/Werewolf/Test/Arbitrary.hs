{-|
Module      : Game.Werewolf.Test.Arbitrary
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Werewolf.Test.Arbitrary (
    -- * Contextual arbitraries
    arbitraryCommand, arbitrarySeeCommand, arbitraryKillVoteCommand, arbitraryLynchVoteCommand,
    arbitraryNewGame, arbitraryPlayer, arbitrarySeer, arbitraryVillager, arbitraryWerewolf,

    -- * Utility functions
    run, run_, runArbitraryCommands,
) where

import Control.Lens         hiding (elements)
import Control.Monad.Except
import Control.Monad.State  hiding (State)
import Control.Monad.Writer

import           Data.Either.Extra
import           Data.List.Extra
import qualified Data.Map          as Map
import           Data.Text         (Text)
import qualified Data.Text         as T

import Game.Werewolf.Command
import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Response
import Game.Werewolf.Role     hiding (Villagers, Werewolves, name, _name)

import Test.QuickCheck

instance Show Command where
    show _ = "command"

arbitraryCommand :: Game -> Gen Command
arbitraryCommand game = case game ^. turn of
    Seers       -> arbitrarySeeCommand game
    Villagers   -> arbitraryLynchVoteCommand game
    Werewolves  -> arbitraryKillVoteCommand game
    NoOne       -> return noopCommand

arbitrarySeeCommand :: Game -> Gen Command
arbitrarySeeCommand game = do
    let applicableCallers   = filter (flip Map.notMember (game ^. sees) . _name) (filterAlive . filterSeers $ game ^. players)
    target                  <- arbitraryPlayer game

    if null applicableCallers
        then return noopCommand
        else elements applicableCallers >>= \caller -> return $ seeCommand (caller ^. name) (target ^. name)

arbitraryKillVoteCommand :: Game -> Gen Command
arbitraryKillVoteCommand game = do
    let applicableCallers   = filter (flip Map.notMember (game ^. votes) . _name) (filterAlive . filterWerewolves $ game ^. players)
    target                  <- arbitraryPlayer game

    if null applicableCallers
        then return noopCommand
        else elements applicableCallers >>= \caller -> return $ killVoteCommand (caller ^. name) (target ^. name)

arbitraryLynchVoteCommand :: Game -> Gen Command
arbitraryLynchVoteCommand game = do
    let applicableCallers   = filter (flip Map.notMember (game ^. votes) . _name) (filterAlive $ game ^. players)
    target                  <- arbitraryPlayer game

    if null applicableCallers
        then return noopCommand
        else elements applicableCallers >>= \caller -> return $ lynchVoteCommand (caller ^. name) (target ^. name)

instance Arbitrary Game where
    arbitrary = do
        game <- arbitraryNewGame
        turn <- arbitrary

        return $ game { _turn = turn }

arbitraryNewGame :: Gen Game
arbitraryNewGame = do
    n <- choose (7, 24)
    players <- nubOn _name <$> infiniteList

    let seer        = head $ filterSeers players
    let werewolves  = take (n `quot` 6 + 1) $ filterWerewolves players
    let villagers   = take (n - 1 - (length werewolves)) $ filterVillagers players

    return $ newGame (seer:werewolves ++ villagers)

instance Arbitrary Turn where
    arbitrary = elements [Seers, Villagers, Werewolves, NoOne]

instance Arbitrary Player where
    arbitrary = newPlayer <$> arbitrary <*> arbitrary

arbitraryPlayer :: Game -> Gen Player
arbitraryPlayer = elements . filterAlive . _players

arbitrarySeer :: Game -> Gen Player
arbitrarySeer = elements . filterAlive . filterSeers . _players

arbitraryVillager :: Game -> Gen Player
arbitraryVillager = elements . filterAlive . filterVillagers . _players

arbitraryWerewolf :: Game -> Gen Player
arbitraryWerewolf = elements . filterAlive . filterWerewolves . _players

instance Arbitrary State where
    arbitrary = elements [Alive, Dead]

instance Arbitrary Role where
    arbitrary = elements allRoles

instance Arbitrary Text where
    arbitrary = T.pack <$> vectorOf 6 (elements ['a'..'z'])

run :: StateT Game (WriterT [Message] (Except [Message])) a -> Game -> Either [Message] (Game, [Message])
run action game = runExcept . runWriterT $ execStateT action game

run_ :: StateT Game (WriterT [Message] (Except [Message])) a -> Game -> Game
run_ action = fst . fromRight . run action

runArbitraryCommands :: Int -> Game -> Gen Game
runArbitraryCommands n = iterateM n $ \game -> do
    command <- arbitraryCommand game

    return $ run_ (apply command) game

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ a = return a
iterateM n f a = f a >>= iterateM (n - 1) f
