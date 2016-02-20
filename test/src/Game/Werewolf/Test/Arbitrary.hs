{-|
Module      : Game.Werewolf.Test.Arbitrary
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Werewolf.Test.Arbitrary (
    -- * Initial arbitraries

    -- ** Game
    NewGame(..),
    GameAtDefendersTurn(..), GameAtGameOver(..), GameAtScapegoatsTurn(..), GameAtSeersTurn(..),
    GameAtSunrise(..), GameAtVillagesTurn(..), GameAtWerewolvesTurn(..), GameAtWildChildsTurn(..),
    GameAtWitchsTurn(..), GameAtWolfHoundsTurn(..),
    GameOnSecondRound(..),
    GameWithAllowedVoters(..), GameWithDeadPlayers(..), GameWithDevourEvent(..),
    GameWithDevourVotes(..), GameWithHeal(..), GameWithLynchVotes(..),
    GameWithOneAllegianceAlive(..), GameWithPoison(..), GameWithProtect(..),
    GameWithProtectAndDevourVotes(..), GameWithRoleModel(..), GameWithRoleModelAtVillagesTurn(..),
    GameWithScapegoatBlamed(..), GameWithSee(..), GameWithVillageIdiotRevealedAtVillagesTurn(..),
    GameWithZeroAllegiancesAlive(..),

    -- ** Player
    arbitraryPlayerSet,

    -- * Contextual arbitraries

    -- ** Command
    arbitraryCommand, arbitraryChooseAllegianceCommand, arbitraryChoosePlayerCommand,
    arbitraryChoosePlayersCommand, arbitraryHealCommand, arbitraryPassCommand,
    arbitraryPoisonCommand, arbitraryProtectCommand, arbitraryQuitCommand, arbitrarySeeCommand,
    arbitraryVoteDevourCommand, arbitraryVoteLynchCommand,
    runArbitraryCommands,

    -- ** Player
    arbitraryPlayer, arbitraryWerewolf,
) where

import Control.Lens hiding (elements)

import           Data.List.Extra
import           Data.Maybe
import           Data.Text       (Text)
import qualified Data.Text       as T

import Game.Werewolf.Command
import Game.Werewolf.Engine          (checkStage)
import Game.Werewolf.Internal.Game
import Game.Werewolf.Internal.Player
import Game.Werewolf.Internal.Role   hiding (name)
import Game.Werewolf.Test.Util

import Prelude hiding (round)

import Test.QuickCheck

instance Arbitrary Game where
    arbitrary = do
        (NewGame game)  <- arbitrary
        stage'          <- arbitrary

        return $ game & stage .~ stage'

instance Arbitrary Stage where
    arbitrary = elements $ allStages \\ [GameOver, Sunrise, Sunset]

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

newtype GameAtScapegoatsTurn = GameAtScapegoatsTurn Game
    deriving (Eq, Show)

instance Arbitrary GameAtScapegoatsTurn where
    arbitrary = do
        (GameWithScapegoatBlamed game) <- arbitrary

        return $ GameAtScapegoatsTurn (run_ checkStage game)

newtype GameAtSeersTurn = GameAtSeersTurn Game
    deriving (Eq, Show)

instance Arbitrary GameAtSeersTurn where
    arbitrary = do
        game <- arbitrary

        return $ GameAtSeersTurn (game & stage .~ SeersTurn)

newtype GameAtSunrise = GameAtSunrise Game
    deriving (Eq, Show)

instance Arbitrary GameAtSunrise where
    arbitrary = do
        game <- arbitrary

        return $ GameAtSunrise (game & stage .~ Sunrise)

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

newtype GameAtWildChildsTurn = GameAtWildChildsTurn Game
    deriving (Eq, Show)

instance Arbitrary GameAtWildChildsTurn where
    arbitrary = do
        game <- arbitrary

        return $ GameAtWildChildsTurn (game & stage .~ WildChildsTurn)

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

newtype GameOnSecondRound = GameOnSecondRound Game
    deriving (Eq, Show)

instance Arbitrary GameOnSecondRound where
    arbitrary = do
        game <- arbitrary

        return $ GameOnSecondRound (game & round .~ 1)

newtype GameWithAllowedVoters = GameWithAllowedVoters Game
    deriving (Eq, Show)

instance Arbitrary GameWithAllowedVoters where
    arbitrary = do
        (GameAtScapegoatsTurn game) <- arbitrary
        (Blind command)             <- arbitraryChoosePlayersCommand game

        return $ GameWithAllowedVoters (run_ (apply command) game)

newtype GameWithDeadPlayers = GameWithDeadPlayers Game
    deriving (Eq, Show)

instance Arbitrary GameWithDeadPlayers where
    arbitrary = do
        game                <- arbitrary
        (NonEmpty players') <- NonEmpty <$> sublistOf (game ^. players)
        let game'           = foldr killPlayer game (players' ^.. names)

        return $ GameWithDeadPlayers (run_ checkStage game')

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

newtype GameWithOneAllegianceAlive = GameWithOneAllegianceAlive Game
    deriving (Eq, Show)

instance Arbitrary GameWithOneAllegianceAlive where
    arbitrary = do
        game            <- arbitrary
        allegiance'     <- arbitrary
        let players'    = filter ((allegiance' /=) . view (role . allegiance)) (game ^. players)
        let game'       = foldr killPlayer game (players' ^.. names)

        return $ GameWithOneAllegianceAlive game'

newtype GameWithPass = GameWithPass Game
    deriving (Eq, Show)

instance Arbitrary GameWithPass where
    arbitrary = do
        game            <- arbitrary
        let game'       = game & stage .~ WitchsTurn
        (Blind command) <- arbitraryPassCommand game'

        return $ GameWithPass (run_ (apply command) game')

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
        (GameWithProtectAndWolfHoundChoice game)    <- arbitrary
        let game'                                   = run_ checkStage game

        GameWithProtectAndDevourVotes <$> runArbitraryCommands (length $ game' ^. players) game'

newtype GameWithProtectAndWolfHoundChoice = GameWithProtectAndWolfHoundChoice Game
    deriving (Eq, Show)

instance Arbitrary GameWithProtectAndWolfHoundChoice where
    arbitrary = do
        (GameWithProtect game)  <- arbitrary
        let game'               = run_ checkStage game
        (Blind command)         <- arbitraryChooseAllegianceCommand game'

        return $ GameWithProtectAndWolfHoundChoice (run_ (apply command) game')

newtype GameWithRoleModel = GameWithRoleModel Game
    deriving (Eq, Show)

instance Arbitrary GameWithRoleModel where
    arbitrary = do
        (GameAtWildChildsTurn game) <- arbitrary
        (Blind command)             <- arbitraryChoosePlayerCommand game
        let game'                   = run_ (apply command) game

        return $ GameWithRoleModel game'

newtype GameWithRoleModelAtVillagesTurn = GameWithRoleModelAtVillagesTurn Game
    deriving (Eq, Show)

instance Arbitrary GameWithRoleModelAtVillagesTurn where
    arbitrary = do
        (GameWithRoleModel game) <- arbitrary

        return $ GameWithRoleModelAtVillagesTurn (game & stage .~ VillagesTurn)

newtype GameWithScapegoatBlamed = GameWithScapegoatBlamed Game
    deriving (Eq, Show)

instance Arbitrary GameWithScapegoatBlamed where
    arbitrary = do
        (GameWithLynchVotes game) <- suchThat arbitrary $ \(GameWithLynchVotes game) -> length (getVoteResult game) > 1

        return $ GameWithScapegoatBlamed game

newtype GameWithSee = GameWithSee Game
    deriving (Eq, Show)

instance Arbitrary GameWithSee where
    arbitrary = do
        game            <- arbitrary
        let game'       = game & stage .~ SeersTurn
        (Blind command) <- arbitrarySeeCommand game'

        return $ GameWithSee (run_ (apply command) game')

newtype GameWithVillageIdiotRevealed = GameWithVillageIdiotRevealed Game
    deriving (Eq, Show)

instance Arbitrary GameWithVillageIdiotRevealed where
    arbitrary = do
        game                <- arbitrary
        let villageIdiot    = game ^?! players . villageIdiots
        let game'           = game & villageIdiotRevealed .~ True & allowedVoters %~ delete (villageIdiot ^. name)

        return $ GameWithVillageIdiotRevealed game'

newtype GameWithVillageIdiotRevealedAtVillagesTurn = GameWithVillageIdiotRevealedAtVillagesTurn Game
    deriving (Eq, Show)

instance Arbitrary GameWithVillageIdiotRevealedAtVillagesTurn where
    arbitrary = do
        (GameWithVillageIdiotRevealed game) <- arbitrary

        return $ GameWithVillageIdiotRevealedAtVillagesTurn (game & stage .~ VillagesTurn)

newtype GameWithZeroAllegiancesAlive = GameWithZeroAllegiancesAlive Game
    deriving (Eq, Show)

instance Arbitrary GameWithZeroAllegiancesAlive where
    arbitrary = do
        game        <- arbitrary
        let game'   = foldr killPlayer game (game ^.. players . names)

        return $ GameWithZeroAllegiancesAlive game'

arbitraryPlayerSet :: Gen [Player]
arbitraryPlayerSet = do
    n <- choose (14, 24)
    players <- nubOn (view name) <$> infiniteList

    let playersWithRestrictedRole = map (\role' -> players ^?! traverse . filteredBy role role') restrictedRoles

    let simpleWerewolves'   = players ^.. taking (n `quot` 6 + 1) simpleWerewolves
    let simpleVillagers'    = players ^.. taking (n - length restrictedRoles - length simpleWerewolves') simpleVillagers

    return $ playersWithRestrictedRole ++ simpleVillagers' ++ simpleWerewolves'

arbitraryCommand :: Game -> Gen (Blind Command)
arbitraryCommand game = case game ^. stage of
    GameOver        -> return $ Blind noopCommand
    DefendersTurn   -> arbitraryProtectCommand game
    ScapegoatsTurn  -> arbitraryChoosePlayersCommand game
    SeersTurn       -> arbitrarySeeCommand game
    Sunrise         -> return $ Blind noopCommand
    Sunset          -> return $ Blind noopCommand
    UrsussGrunt     -> return $ Blind noopCommand
    VillagesTurn    -> arbitraryVoteLynchCommand game
    WerewolvesTurn  -> arbitraryVoteDevourCommand game
    WildChildsTurn  -> arbitraryChoosePlayerCommand game
    WitchsTurn      -> oneof [
        arbitraryHealCommand game,
        arbitraryPassCommand game,
        arbitraryPoisonCommand game
        ]
    WolfHoundsTurn  -> arbitraryChooseAllegianceCommand game

arbitraryChooseAllegianceCommand :: Game -> Gen (Blind Command)
arbitraryChooseAllegianceCommand game = do
    let wolfHound   = game ^?! players . wolfHounds
    allegianceName  <- elements $ map (T.pack . show) [Villagers, Werewolves]

    return . Blind $ chooseAllegianceCommand (wolfHound ^. name) allegianceName

arbitraryChoosePlayerCommand :: Game -> Gen (Blind Command)
arbitraryChoosePlayerCommand game = do
    let wildChild   = game ^?! players . wildChildren
    target          <- suchThat (arbitraryPlayer game) (wildChild /=)

    return . Blind $ choosePlayerCommand (wildChild ^. name) (target ^. name)

arbitraryChoosePlayersCommand :: Game -> Gen (Blind Command)
arbitraryChoosePlayersCommand game = do
    let scapegoat       = game ^?! players . scapegoats
    (NonEmpty players') <- NonEmpty <$> sublistOf (game ^.. players . traverse . alive)

    return . Blind $ choosePlayersCommand (scapegoat ^. name) (players' ^.. names)

arbitraryHealCommand :: Game -> Gen (Blind Command)
arbitraryHealCommand game = do
    let witch = game ^?! players . witches

    return $ if game ^. healUsed
        then Blind noopCommand
        else seq (fromJust $ getDevourEvent game) (Blind $ healCommand (witch ^. name))

arbitraryPassCommand :: Game -> Gen (Blind Command)
arbitraryPassCommand game = do
    let witch = game ^?! players . witches

    return . Blind $ passCommand (witch ^. name)

arbitraryPoisonCommand :: Game -> Gen (Blind Command)
arbitraryPoisonCommand game = do
    let witch   = game ^?! players . witches
    target      <- arbitraryPlayer game

    return $ if isJust (game ^. poison)
        then Blind noopCommand
        else Blind $ poisonCommand (witch ^. name) (target ^. name)

arbitraryProtectCommand :: Game -> Gen (Blind Command)
arbitraryProtectCommand game = do
    let defender    = game ^?! players . defenders
    -- TODO (hjw): add suchThat (/= priorProtect)
    target          <- suchThat (arbitraryPlayer game) (defender /=)

    return $ if isJust (game ^. protect)
        then Blind noopCommand
        else Blind $ protectCommand (defender ^. name) (target ^. name)

arbitraryQuitCommand :: Game -> Gen (Blind Command)
arbitraryQuitCommand game = do
    let applicableCallers = game ^.. players . traverse . alive

    if null applicableCallers
        then return $ Blind noopCommand
        else elements applicableCallers >>= \caller ->
            return . Blind $ quitCommand (caller ^. name)

arbitrarySeeCommand :: Game -> Gen (Blind Command)
arbitrarySeeCommand game = do
    let seer    = game ^?! players . seers
    target      <- arbitraryPlayer game

    return $ if isJust (game ^. see)
        then Blind noopCommand
        else Blind $ seeCommand (seer ^. name) (target ^. name)

arbitraryVoteDevourCommand :: Game -> Gen (Blind Command)
arbitraryVoteDevourCommand game = do
    let applicableCallers   = getPendingVoters game ^.. werewolves
    target                  <- suchThat (arbitraryPlayer game) $ isn't werewolf

    if null applicableCallers
        then return $ Blind noopCommand
        else elements applicableCallers >>= \caller ->
            return . Blind $ voteDevourCommand (caller ^. name) (target ^. name)

arbitraryVoteLynchCommand :: Game -> Gen (Blind Command)
arbitraryVoteLynchCommand game = do
    let applicableCallers   = getAllowedVoters game `intersect` getPendingVoters game
    target                  <- arbitraryPlayer game

    if null applicableCallers
        then return $ Blind noopCommand
        else elements applicableCallers >>= \caller ->
            return . Blind $ voteLynchCommand (caller ^. name) (target ^. name)

runArbitraryCommands :: Int -> Game -> Gen Game
runArbitraryCommands n = iterateM n $ \game -> do
    (Blind command) <- arbitraryCommand game

    return $ run_ (apply command) game

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ a = return a
iterateM n f a = f a >>= iterateM (n - 1) f

arbitraryPlayer :: Game -> Gen Player
arbitraryPlayer game = elements $ game ^.. players . traverse . alive

arbitraryWerewolf :: Game -> Gen Player
arbitraryWerewolf game = elements $ game ^.. players . werewolves . alive
