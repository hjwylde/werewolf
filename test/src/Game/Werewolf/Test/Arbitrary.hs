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
    arbitraryCommand, arbitraryDevourVoteCommand, arbitraryHealCommand, arbitraryLynchVoteCommand,
    arbitraryPassCommand, arbitraryPoisonCommand, arbitraryQuitCommand, arbitrarySeeCommand,
    arbitraryNewGame, arbitraryPlayer, arbitraryPlayerSet, arbitraryScapegoat, arbitrarySeer,
    arbitraryVillager, arbitraryWerewolf, arbitraryWitch,

    -- * Utility functions
    run, run_, runArbitraryCommands,
) where

import Control.Lens         hiding (elements)
import Control.Monad.Except
import Control.Monad.State  hiding (State)
import Control.Monad.Writer

import           Data.Either.Extra
import           Data.List.Extra
import           Data.Maybe
import           Data.Text         (Text)
import qualified Data.Text         as T

import Game.Werewolf.Command
import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Response
import Game.Werewolf.Role     hiding (name, _name)

import Test.QuickCheck

instance Show Command where
    show _ = "command"

arbitraryCommand :: Game -> Gen Command
arbitraryCommand game = case game ^. stage of
    GameOver        -> return noopCommand
    Sunrise         -> return noopCommand
    Sunset          -> return noopCommand
    SeersTurn       -> arbitrarySeeCommand game
    VillagesTurn    -> arbitraryLynchVoteCommand game
    WerewolvesTurn  -> arbitraryDevourVoteCommand game
    WitchsTurn      -> oneof [
        arbitraryHealCommand game,
        arbitraryPassCommand game,
        arbitraryPoisonCommand game
        ]

arbitraryDevourVoteCommand :: Game -> Gen Command
arbitraryDevourVoteCommand game = do
    let applicableCallers   = filterWerewolves $ getPendingVoters game
    target                  <- suchThat (arbitraryPlayer game) $ not . isWerewolf

    if null applicableCallers
        then return noopCommand
        else elements applicableCallers >>= \caller -> return $ devourVoteCommand (caller ^. name) (target ^. name)

arbitraryLynchVoteCommand :: Game -> Gen Command
arbitraryLynchVoteCommand game = do
    let applicableCallers   = getPendingVoters game
    target                  <- arbitraryPlayer game

    if null applicableCallers
        then return noopCommand
        else elements applicableCallers >>= \caller -> return $ lynchVoteCommand (caller ^. name) (target ^. name)

arbitraryHealCommand :: Game -> Gen Command
arbitraryHealCommand game = do
    let witchName = (head . filterWitches $ game ^. players) ^. name

    return $ if game ^. healUsed
        then noopCommand
        else seq (fromJust (getDevourEvent game)) $ healCommand witchName

arbitraryPassCommand :: Game -> Gen Command
arbitraryPassCommand game = do
    witch <- arbitraryWitch game

    return $ passCommand (witch ^. name)

arbitraryPoisonCommand :: Game -> Gen Command
arbitraryPoisonCommand game = do
    let witch   = head . filterWitches $ game ^. players
    target      <- arbitraryPlayer game

    return $ if isJust (game ^. poison)
        then noopCommand
        else poisonCommand (witch ^. name) (target ^. name)

arbitraryQuitCommand :: Game -> Gen Command
arbitraryQuitCommand game = do
    let applicableCallers = filterAlive $ game ^. players

    if null applicableCallers
        then return noopCommand
        else elements applicableCallers >>= \caller -> return $ quitCommand (caller ^. name)

arbitrarySeeCommand :: Game -> Gen Command
arbitrarySeeCommand game = do
    let seer    = head . filterSeers $ game ^. players
    target      <- arbitraryPlayer game

    return $ if isJust (game ^. see)
        then noopCommand
        else seeCommand (seer ^. name) (target ^. name)

instance Arbitrary Game where
    arbitrary = do
        game <- arbitraryNewGame
        stage <- arbitrary

        return $ game { _stage = stage }

arbitraryNewGame :: Gen Game
arbitraryNewGame = newGame <$> arbitraryPlayerSet

instance Arbitrary Stage where
    arbitrary = elements [GameOver, SeersTurn, VillagesTurn, WerewolvesTurn, WitchsTurn]

instance Arbitrary Player where
    arbitrary = newPlayer <$> arbitrary <*> arbitrary

arbitraryPlayer :: Game -> Gen Player
arbitraryPlayer = elements . filterAlive . _players

arbitraryPlayerSet :: Gen [Player]
arbitraryPlayerSet = do
    n <- choose (7, 24)
    players <- nubOn _name <$> infiniteList

    let scapegoat   = head $ filterScapegoats players
    let seer        = head $ filterSeers players
    let witch       = head $ filterWitches players

    let werewolves  = take (n `quot` 6 + 1) $ filterWerewolves players
    let villagers   = take (n - 3 - (length werewolves)) $ filterVillagers players

    return $ scapegoat:seer:witch:werewolves ++ villagers

arbitraryScapegoat :: Game -> Gen Player
arbitraryScapegoat = elements . filterAlive . filterScapegoats . _players

arbitrarySeer :: Game -> Gen Player
arbitrarySeer = elements . filterAlive . filterSeers . _players

arbitraryVillager :: Game -> Gen Player
arbitraryVillager = elements . filterAlive . filterVillagers . _players

arbitraryWerewolf :: Game -> Gen Player
arbitraryWerewolf = elements . filterAlive . filterWerewolves . _players

arbitraryWitch :: Game -> Gen Player
arbitraryWitch = elements . filterAlive . filterWitches . _players

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
