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
    NewGame(..),
    GameAtDefendersTurn(..), GameAtGameOver(..), GameAtSeersTurn(..), GameAtVillagesTurn(..),
    GameAtWerewolvesTurn(..), GameAtWitchsTurn(..), GameAtWolfHoundsTurn(..),
    GameWithDevourEvent(..), GameWithDevourVotes(..), GameWithHeal(..), GameWithLynchVotes(..),
    GameWithPoison(..), GameWithProtect(..), GameWithProtectAndDevourVotes(..), GameWithSee(..),

    -- ** Player
    arbitraryPlayerSet,

    -- * Contextual arbitraries

    -- ** Command
    arbitraryCommand, arbitraryChooseCommand, arbitraryDevourVoteCommand, arbitraryHealCommand,
    arbitraryLynchVoteCommand, arbitraryPassCommand, arbitraryPoisonCommand,
    arbitraryProtectCommand, arbitraryQuitCommand, arbitrarySeeCommand,
    runArbitraryCommands,

    -- ** Player
    arbitraryPlayer, arbitraryPlayerAlignedWithWerewolves,
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
import Game.Werewolf.Role      hiding (name)
import Game.Werewolf.Test.Util

import Test.QuickCheck

instance Arbitrary Game where
    arbitrary = do
        (NewGame game)  <- arbitrary
        stage'          <- arbitrary

        return $ game & stage .~ stage'

instance Arbitrary Stage where
    arbitrary = elements
        [GameOver, DefendersTurn, SeersTurn, VillagesTurn, WerewolvesTurn, WitchsTurn, WolfHoundsTurn]

instance Arbitrary Player where
    arbitrary = newPlayer <$> arbitrary <*> arbitrary

instance Arbitrary State where
    arbitrary = elements [Alive, Dead]

instance Arbitrary Role where
    arbitrary = elements allRoles

instance Arbitrary Allegiance where
    arbitrary = elements allAllegiances

instance Arbitrary Text where
    arbitrary = T.pack <$> vectorOf 6 (elements ['a'..'z'])

newtype NewGame = NewGame Game
    deriving (Eq, Show)

instance Arbitrary NewGame where
    arbitrary = NewGame . newGame <$> arbitraryPlayerSet

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

newtype GameAtSeersTurn = GameAtSeersTurn Game
    deriving (Eq, Show)

instance Arbitrary GameAtSeersTurn where
    arbitrary = do
        game <- arbitrary

        return $ GameAtSeersTurn (game & stage .~ SeersTurn)

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

newtype GameAtWolfHoundsTurn = GameAtWolfHoundsTurn Game
    deriving (Eq, Show)

instance Arbitrary GameAtWolfHoundsTurn where
    arbitrary = do
        game <- arbitrary

        return $ GameAtWolfHoundsTurn (game & stage .~ WolfHoundsTurn)

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
        (Blind command)             <- arbitraryHealCommand game

        return $ GameWithHeal (run_ (apply command) game)

newtype GameWithLynchVotes = GameWithLynchVotes Game
    deriving (Eq, Show)

instance Arbitrary GameWithLynchVotes where
    arbitrary = do
        game <- arbitrary

        GameWithLynchVotes <$> runArbitraryCommands (length $ game ^. players) (game & stage .~ VillagesTurn)

newtype GameWithPoison = GameWithPoison Game
    deriving (Eq, Show)

instance Arbitrary GameWithPoison where
    arbitrary = do
        game            <- arbitrary
        let game'       = game & stage .~ WitchsTurn
        (Blind command) <- arbitraryPoisonCommand game'

        return $ GameWithPoison (run_ (apply command) game')

newtype GameWithProtect = GameWithProtect Game
    deriving (Eq, Show)

instance Arbitrary GameWithProtect where
    arbitrary = do
        game            <- arbitrary
        let game'       = game & stage .~ DefendersTurn
        (Blind command) <- arbitraryProtectCommand game'

        return $ GameWithProtect (run_ (apply command) game')

newtype GameWithProtectAndDevourVotes = GameWithProtectAndDevourVotes Game
    deriving (Eq, Show)

instance Arbitrary GameWithProtectAndDevourVotes where
    arbitrary = do
        (GameWithProtect game)  <- arbitrary
        let game'               = run_ checkStage game

        GameWithProtectAndDevourVotes <$> runArbitraryCommands (length $ game' ^. players) game'

newtype GameWithSee = GameWithSee Game
    deriving (Eq, Show)

instance Arbitrary GameWithSee where
    arbitrary = do
        game            <- arbitrary
        let game'       = game & stage .~ SeersTurn
        (Blind command) <- arbitrarySeeCommand game'

        return $ GameWithSee (run_ (apply command) game')

arbitraryPlayerSet :: Gen [Player]
arbitraryPlayerSet = do
    n <- choose (10, 24)
    players <- nubOn (view name) <$> infiniteList

    let playersWithRestrictedRole = map (\role' -> head $ filter ((role' ==) . view role) players) restrictedRoles

    let werewolves  = take (n `quot` 6 + 1) $ filterWerewolves players
    let villagers   = take (n - length restrictedRoles - length werewolves) $ filterVillagers players

    return $ playersWithRestrictedRole ++ werewolves ++ villagers

arbitraryCommand :: Game -> Gen (Blind Command)
arbitraryCommand game = case game ^. stage of
    GameOver        -> return $ Blind noopCommand
    DefendersTurn   -> arbitraryProtectCommand game
    Sunrise         -> return $ Blind noopCommand
    Sunset          -> return $ Blind noopCommand
    SeersTurn       -> arbitrarySeeCommand game
    VillagesTurn    -> arbitraryLynchVoteCommand game
    WerewolvesTurn  -> arbitraryDevourVoteCommand game
    WitchsTurn      -> oneof [
        arbitraryHealCommand game,
        arbitraryPassCommand game,
        arbitraryPoisonCommand game
        ]
    WolfHoundsTurn  -> arbitraryChooseCommand game

arbitraryChooseCommand :: Game -> Gen (Blind Command)
arbitraryChooseCommand game = do
    let wolfHound   = head . filterWolfHounds $ game ^. players
    allegiance      <- elements [Villagers, Werewolves]

    return . Blind $ chooseCommand (wolfHound ^. name) (allegiance)

arbitraryDevourVoteCommand :: Game -> Gen (Blind Command)
arbitraryDevourVoteCommand game = do
    let applicableCallers   = filterAlignedWithWerewolves $ getPendingVoters game
    target                  <- suchThat (arbitraryPlayer game) $ not . isAlignedWithWerewolves

    if null applicableCallers
        then return $ Blind noopCommand
        else elements applicableCallers >>= \caller ->
            return . Blind $ devourVoteCommand (caller ^. name) (target ^. name)

arbitraryLynchVoteCommand :: Game -> Gen (Blind Command)
arbitraryLynchVoteCommand game = do
    let applicableCallers   = getPendingVoters game
    target                  <- arbitraryPlayer game

    if null applicableCallers
        then return $ Blind noopCommand
        else elements applicableCallers >>= \caller ->
            return . Blind $ lynchVoteCommand (caller ^. name) (target ^. name)

arbitraryHealCommand :: Game -> Gen (Blind Command)
arbitraryHealCommand game = do
    let witch = head . filterWitches $ game ^. players

    return $ if game ^. healUsed
        then Blind noopCommand
        else seq (fromJust $ getDevourEvent game) (Blind $ healCommand (witch ^. name))

arbitraryPassCommand :: Game -> Gen (Blind Command)
arbitraryPassCommand game = do
    let witch = head . filterWitches $ game ^. players

    return . Blind $ passCommand (witch ^. name)

arbitraryPoisonCommand :: Game -> Gen (Blind Command)
arbitraryPoisonCommand game = do
    let witch   = head . filterWitches $ game ^. players
    target      <- arbitraryPlayer game

    return $ if isJust (game ^. poison)
        then Blind noopCommand
        else Blind $ poisonCommand (witch ^. name) (target ^. name)

arbitraryProtectCommand :: Game -> Gen (Blind Command)
arbitraryProtectCommand game = do
    let defender    = head . filterDefenders $ game ^. players
    -- TODO (hjw): add suchThat (/= priorProtect)
    target          <- suchThat (arbitraryPlayer game) (defender /=)

    return $ if isJust (game ^. protect)
        then Blind noopCommand
        else Blind $ protectCommand (defender ^. name) (target ^. name)

arbitraryQuitCommand :: Game -> Gen (Blind Command)
arbitraryQuitCommand game = do
    let applicableCallers = filterAlive $ game ^. players

    if null applicableCallers
        then return $ Blind noopCommand
        else elements applicableCallers >>= \caller ->
            return . Blind $ quitCommand (caller ^. name)

arbitrarySeeCommand :: Game -> Gen (Blind Command)
arbitrarySeeCommand game = do
    let seer    = head . filterSeers $ game ^. players
    target      <- arbitraryPlayer game

    return $ if isJust (game ^. see)
        then Blind noopCommand
        else Blind $ seeCommand (seer ^. name) (target ^. name)

runArbitraryCommands :: Int -> Game -> Gen Game
runArbitraryCommands n = iterateM n $ \game -> do
    (Blind command) <- arbitraryCommand game

    return $ run_ (apply command) game

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ a = return a
iterateM n f a = f a >>= iterateM (n - 1) f

arbitraryPlayer :: Game -> Gen Player
arbitraryPlayer = elements . filterAlive . view players

arbitraryPlayerAlignedWithWerewolves :: Game -> Gen Player
arbitraryPlayerAlignedWithWerewolves =
    elements . filterAlive . filterAlignedWithWerewolves . view players
