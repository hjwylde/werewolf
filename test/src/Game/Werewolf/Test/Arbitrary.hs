{-|
Module      : Game.Werewolf.Test.Arbitrary
Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Werewolf.Test.Arbitrary (
    -- * Initial arbitraries

    -- ** Game
    GameAtDefendersTurn(..), GameAtGameOver(..), GameAtVillagesTurn(..), GameAtWerewolvesTurn(..),
    GameAtWitchsTurn(..), GameAtSeersTurn(..),
    GameWithDevourEvent(..), GameWithDevourVotes(..), GameWithHeal(..), GameWithLynchVotes(..),
    GameWithPoison(..), GameWithProtect(..), GameWithProtectAndDevourVotes(..),

    -- ** Player
    arbitraryPlayerSet,

    -- * Contextual arbitraries

    -- ** Command
    arbitraryCommand, arbitraryDevourVoteCommand, arbitraryHealCommand, arbitraryLynchVoteCommand,
    arbitraryPassCommand, arbitraryPoisonCommand, arbitraryProtectCommand, arbitraryQuitCommand,
    arbitrarySeeCommand, runArbitraryCommands,

    -- ** Player
    arbitraryPlayer, arbitraryWerewolf,
) where

import Control.Lens hiding (elements)

import           Data.List.Extra
import           Data.Maybe
import           Data.Text       (Text)
import qualified Data.Text       as T

import Game.Werewolf.Command
import Game.Werewolf.Engine    (checkStage)
import Game.Werewolf.Game
import Game.Werewolf.Player
import Game.Werewolf.Role      hiding (name, _name)
import Game.Werewolf.Test.Util

import Test.QuickCheck

instance Show Command where
    show _ = "command"

instance Arbitrary Game where
    arbitrary = do
        game    <- newGame <$> arbitraryPlayerSet
        stage   <- arbitrary

        return $ game { _stage = stage }

instance Arbitrary Stage where
    arbitrary = elements
        [GameOver, DefendersTurn, SeersTurn, VillagesTurn, WerewolvesTurn, WitchsTurn]

instance Arbitrary Player where
    arbitrary = newPlayer <$> arbitrary <*> arbitrary

instance Arbitrary State where
    arbitrary = elements [Alive, Dead]

instance Arbitrary Role where
    arbitrary = elements allRoles

instance Arbitrary Text where
    arbitrary = T.pack <$> vectorOf 6 (elements ['a'..'z'])

newtype GameAtDefendersTurn = GameAtDefendersTurn Game
    deriving (Eq, Show)

instance Arbitrary GameAtDefendersTurn where
    arbitrary = do
        game <- arbitrary

        return $ GameAtDefendersTurn (game & stage .~ DefendersTurn)

newtype GameAtGameOver = GameAtGameOver Game
    deriving (Eq, Show)

instance Arbitrary GameAtGameOver where
    arbitrary = do
        game <- arbitrary

        return $ GameAtGameOver (game & stage .~ GameOver)

newtype GameAtVillagesTurn = GameAtVillagesTurn Game
    deriving (Eq, Show)

instance Arbitrary GameAtVillagesTurn where
    arbitrary = do
        game <- arbitrary

        return $ GameAtVillagesTurn (game & stage .~ VillagesTurn)

newtype GameAtWerewolvesTurn = GameAtWerewolvesTurn Game
    deriving (Eq, Show)

instance Arbitrary GameAtWerewolvesTurn where
    arbitrary = do
        game <- arbitrary

        return $ GameAtWerewolvesTurn (game & stage .~ WerewolvesTurn)

newtype GameAtWitchsTurn = GameAtWitchsTurn Game
    deriving (Eq, Show)

instance Arbitrary GameAtWitchsTurn where
    arbitrary = do
        game <- arbitrary

        return $ GameAtWitchsTurn (game & stage .~ WitchsTurn)

newtype GameAtSeersTurn = GameAtSeersTurn Game
    deriving (Eq, Show)

instance Arbitrary GameAtSeersTurn where
    arbitrary = do
        game <- arbitrary

        return $ GameAtSeersTurn (game & stage .~ SeersTurn)

newtype GameWithDevourEvent = GameWithDevourEvent Game
    deriving (Eq, Show)

instance Arbitrary GameWithDevourEvent where
    arbitrary = do
        (GameWithDevourVotes game) <- suchThat arbitrary $ \(GameWithDevourVotes game) ->
            length (getVoteResult game) == 1

        return $ GameWithDevourEvent (run_ checkStage game)

newtype GameWithDevourVotes = GameWithDevourVotes Game
    deriving (Eq, Show)

instance Arbitrary GameWithDevourVotes where
    arbitrary = do
        game <- arbitrary

        GameWithDevourVotes <$> runArbitraryCommands (length $ game ^. players) (game & stage .~ WerewolvesTurn)

newtype GameWithHeal = GameWithHeal Game
    deriving (Eq, Show)

instance Arbitrary GameWithHeal where
    arbitrary = do
        (GameWithDevourEvent game)  <- arbitrary
        command                     <- arbitraryHealCommand game

        return $ GameWithHeal (run_ (apply command) game)

newtype GameWithLynchVotes = GameWithLynchVotes Game
    deriving (Eq, Show)

instance Arbitrary GameWithLynchVotes where
    arbitrary = do
        game <- arbitrary

        GameWithLynchVotes <$> runArbitraryCommands (length $ game ^. players) (game { _stage = VillagesTurn })

newtype GameWithProtect = GameWithProtect Game
    deriving (Eq, Show)

instance Arbitrary GameWithProtect where
    arbitrary = do
        game        <- arbitrary
        let game'   = game & stage .~ DefendersTurn
        command     <- arbitraryProtectCommand game'

        return $ GameWithProtect (run_ (apply command) game')

newtype GameWithProtectAndDevourVotes = GameWithProtectAndDevourVotes Game
    deriving (Eq, Show)

instance Arbitrary GameWithProtectAndDevourVotes where
    arbitrary = do
        (GameWithProtect game)  <- arbitrary
        let game'               = run_ checkStage game

        GameWithProtectAndDevourVotes <$> runArbitraryCommands (length $ game' ^. players) game'

newtype GameWithPoison = GameWithPoison Game
    deriving (Eq, Show)

instance Arbitrary GameWithPoison where
    arbitrary = do
        game        <- arbitrary
        let game'   = game & stage .~ WitchsTurn
        command     <- arbitraryPoisonCommand game'

        return $ GameWithPoison (run_ (apply command) game')

arbitraryPlayerSet :: Gen [Player]
arbitraryPlayerSet = do
    n <- choose (10, 24)
    players <- nubOn _name <$> infiniteList

    let defender            = head $ filterDefenders players
    let scapegoat           = head $ filterScapegoats players
    let seer                = head $ filterSeers players
    let villagerVillager    = head $ filterVillagerVillagers players
    let witch               = head $ filterWitches players

    let werewolves  = take (n `quot` 6 + 1) $ filterWerewolves players
    let villagers   = take (n - 5 - (length werewolves)) $ filterVillagers players

    return $ defender:scapegoat:seer:villagerVillager:witch:werewolves ++ villagers

arbitraryCommand :: Game -> Gen Command
arbitraryCommand game = case game ^. stage of
    GameOver        -> return noopCommand
    DefendersTurn   -> arbitraryProtectCommand game
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
        else elements applicableCallers >>= \caller ->
            return $ devourVoteCommand (caller ^. name) (target ^. name)

arbitraryLynchVoteCommand :: Game -> Gen Command
arbitraryLynchVoteCommand game = do
    let applicableCallers   = getPendingVoters game
    target                  <- arbitraryPlayer game

    if null applicableCallers
        then return noopCommand
        else elements applicableCallers >>= \caller ->
            return $ lynchVoteCommand (caller ^. name) (target ^. name)

arbitraryHealCommand :: Game -> Gen Command
arbitraryHealCommand game = do
    let witch = head . filterWitches $ game ^. players

    return $ if game ^. healUsed
        then noopCommand
        else seq (fromJust (getDevourEvent game)) $ healCommand (witch ^. name)

arbitraryPassCommand :: Game -> Gen Command
arbitraryPassCommand game = do
    let witch = head . filterWitches $ game ^. players

    return $ passCommand (witch ^. name)

arbitraryPoisonCommand :: Game -> Gen Command
arbitraryPoisonCommand game = do
    let witch   = head . filterWitches $ game ^. players
    target      <- arbitraryPlayer game

    return $ if isJust (game ^. poison)
        then noopCommand
        else poisonCommand (witch ^. name) (target ^. name)

arbitraryProtectCommand :: Game -> Gen Command
arbitraryProtectCommand game = do
    let defender    = head . filterDefenders $ game ^. players
    target          <- suchThat (arbitraryPlayer game) (defender /=)

    return $ if isJust (game ^. protect)
        then noopCommand
        else protectCommand (defender ^. name) (target ^. name)

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

runArbitraryCommands :: Int -> Game -> Gen Game
runArbitraryCommands n = iterateM n $ \game -> do
    command <- arbitraryCommand game

    return $ run_ (apply command) game

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ a = return a
iterateM n f a = f a >>= iterateM (n - 1) f

arbitraryPlayer :: Game -> Gen Player
arbitraryPlayer = elements . filterAlive . _players

arbitraryWerewolf :: Game -> Gen Player
arbitraryWerewolf = elements . filterAlive . filterWerewolves . _players
