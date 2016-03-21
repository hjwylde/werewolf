{-|
Module      : Game.Werewolf.Test.Arbitrary
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Werewolf.Test.Arbitrary (
    -- * Initial arbitraries

    -- ** Game
    NewGame(..),
    GameAtDefendersTurn(..), GameAtDevotedServantsTurn(..), GameAtGameOver(..),
    GameAtScapegoatsTurn(..), GameAtSeersTurn(..), GameAtSunrise(..), GameAtVillagesTurn(..),
    GameAtWerewolvesTurn(..), GameAtWildChildsTurn(..), GameAtWitchsTurn(..),
    GameAtWolfHoundsTurn(..),
    GameOnSecondRound(..),
    GameWithAllegianceChosen(..), GameWithAllowedVoters(..), GameWithConflictingVote(..),
    GameWithDeadPlayers(..), GameWithDevourEvent(..), GameWithDevourVotes(..), GameWithHeal(..),
    GameWithLynchVotes(..), GameWithMajorityVote(..), GameWithOneAllegianceAlive(..),
    GameWithPassAtDevotedServantsTurn(..), GameWithPoison(..), GameWithProtect(..),
    GameWithProtectAndDevourVotes(..), GameWithRoleModel(..), GameWithRoleModelAtVillagesTurn(..),
    GameWithSee(..), GameWithVillageIdiotRevealedAtVillagesTurn(..),

    -- ** Player
    arbitraryPlayerSet,

    -- * Contextual arbitraries

    -- ** Command
    arbitraryCommand, arbitraryChooseAllegianceCommand, arbitraryChoosePlayerCommand,
    arbitraryChoosePlayersCommand, arbitraryHealCommand, arbitraryPassDevotedServantsTurnCommand,
    arbitraryPassWitchsTurnCommand, arbitraryPoisonCommand, arbitraryProtectCommand,
    arbitraryQuitCommand, arbitraryRevealCommand, arbitrarySeeCommand, arbitraryVoteDevourCommand,
    arbitraryVoteLynchCommand,
    runArbitraryCommands,

    -- ** Player
    arbitraryPlayer, arbitraryWerewolf,
) where

import Control.Lens hiding (elements, isn't)

import           Data.List.Extra
import           Data.Maybe
import           Data.Text       (Text)
import qualified Data.Text       as T

import Game.Werewolf
import Game.Werewolf.Command.Defender
import Game.Werewolf.Command.Witch
import Game.Werewolf.Test.Util

import Prelude hiding (round)

import Test.QuickCheck

instance Arbitrary Game where
    arbitrary = do
        (NewGame game)  <- arbitrary
        stage'          <- arbitrary

        return $ game & stage .~ stage'

instance Arbitrary Stage where
    arbitrary = elements . nub $ allStages \\ [GameOver, Sunrise, Sunset]

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

newtype GameAtDevotedServantsTurn = GameAtDevotedServantsTurn Game
    deriving (Eq, Show)

instance Arbitrary GameAtDevotedServantsTurn where
    arbitrary = do
        (GameWithMajorityVote game) <- suchThat arbitrary $ \(GameWithMajorityVote game) ->
            isn't devotedServant (head $ getVoteResult game)

        return $ GameAtDevotedServantsTurn (run_ checkStage game)

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
        (GameWithConflictingVote game) <- arbitrary

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

newtype GameWithAllegianceChosen = GameWithAllegianceChosen Game
    deriving (Eq, Show)

instance Arbitrary GameWithAllegianceChosen where
    arbitrary = do
        (GameAtWolfHoundsTurn game) <- arbitrary
        (Blind command)             <- arbitraryChooseAllegianceCommand game

        return $ GameWithAllegianceChosen (run_ (apply command) game)

newtype GameWithAllowedVoters = GameWithAllowedVoters Game
    deriving (Eq, Show)

instance Arbitrary GameWithAllowedVoters where
    arbitrary = do
        (GameAtScapegoatsTurn game) <- arbitrary
        (Blind command)             <- arbitraryChoosePlayersCommand game

        return $ GameWithAllowedVoters (run_ (apply command) game)

newtype GameWithConflictingVote = GameWithConflictingVote Game
    deriving (Eq, Show)

instance Arbitrary GameWithConflictingVote where
    arbitrary = do
        (GameWithLynchVotes game) <- suchThat arbitrary $ \(GameWithLynchVotes game) ->
            length (getVoteResult game) > 1

        return $ GameWithConflictingVote game

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

newtype GameWithMajorityVote = GameWithMajorityVote Game
    deriving (Eq, Show)

instance Arbitrary GameWithMajorityVote where
    arbitrary = do
        (GameWithLynchVotes game) <- suchThat arbitrary $ \(GameWithLynchVotes game) ->
            length (getVoteResult game) == 1

        return $ GameWithMajorityVote game

newtype GameWithMajorityVoteAtDevotedServantsTurn = GameWithMajorityVoteAtDevotedServantsTurn Game
    deriving (Eq, Show)

instance Arbitrary GameWithMajorityVoteAtDevotedServantsTurn where
    arbitrary = do
        (GameWithMajorityVote game) <- arbitrary

        return . GameWithMajorityVoteAtDevotedServantsTurn $ run_ checkStage game

newtype GameWithOneAllegianceAlive = GameWithOneAllegianceAlive Game
    deriving (Eq, Show)

instance Arbitrary GameWithOneAllegianceAlive where
    arbitrary = do
        game            <- arbitrary
        allegiance'     <- elements [Villagers, Werewolves]
        let playerNames = game ^.. players . traverse . filteredBy (role . allegiance) allegiance' . name
        let game'       = foldr killPlayer game (game ^.. players . names \\ playerNames)

        return $ GameWithOneAllegianceAlive game'

newtype GameWithPassAtDevotedServantsTurn = GameWithPassAtDevotedServantsTurn Game
    deriving (Eq, Show)

instance Arbitrary GameWithPassAtDevotedServantsTurn where
    arbitrary = do
        (GameAtDevotedServantsTurn game)    <- arbitrary
        (Blind command)                     <- arbitraryPassDevotedServantsTurnCommand game

        return $ GameWithPassAtDevotedServantsTurn (run_ (apply command) game)

newtype GameWithPassAtWitchsTurn = GameWithPassAtWitchsTurn Game
    deriving (Eq, Show)

instance Arbitrary GameWithPassAtWitchsTurn where
    arbitrary = do
        game            <- arbitrary
        let game'       = game & stage .~ WitchsTurn
        (Blind command) <- arbitraryPassWitchsTurnCommand game'

        return $ GameWithPassAtWitchsTurn (run_ (apply command) game')

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
        game                    <- arbitrary
        let villageIdiotsName   = game ^?! players . villageIdiots . name
        let game'               = game & villageIdiotRevealed .~ True & allowedVoters %~ delete villageIdiotsName

        return $ GameWithVillageIdiotRevealed game'

newtype GameWithVillageIdiotRevealedAtVillagesTurn = GameWithVillageIdiotRevealedAtVillagesTurn Game
    deriving (Eq, Show)

instance Arbitrary GameWithVillageIdiotRevealedAtVillagesTurn where
    arbitrary = do
        (GameWithVillageIdiotRevealed game) <- arbitrary

        return $ GameWithVillageIdiotRevealedAtVillagesTurn (game & stage .~ VillagesTurn)

arbitraryPlayerSet :: Gen [Player]
arbitraryPlayerSet = do
    n       <- choose (14, 24)
    players <- nubOn (view name) <$> infiniteList

    let playersWithRestrictedRole = map (\role' -> players ^?! traverse . filteredBy role role') restrictedRoles

    let simpleWerewolves'   = players ^.. taking (n `quot` 5 + 1) simpleWerewolves
    let simpleVillagers'    = players ^.. taking (n - length restrictedRoles - length simpleWerewolves') simpleVillagers

    return $ playersWithRestrictedRole ++ simpleVillagers' ++ simpleWerewolves'

arbitraryCommand :: Game -> Gen (Blind Command)
arbitraryCommand game = case game ^. stage of
    DefendersTurn       -> arbitraryProtectCommand game
    DevotedServantsTurn -> oneof
        [ arbitraryPassDevotedServantsTurnCommand game
        , arbitraryRevealCommand game
        ]
    GameOver            -> return $ Blind noopCommand
    Lynching            -> return $ Blind noopCommand
    ScapegoatsTurn      -> arbitraryChoosePlayersCommand game
    SeersTurn           -> arbitrarySeeCommand game
    Sunrise             -> return $ Blind noopCommand
    Sunset              -> return $ Blind noopCommand
    UrsussGrunt         -> return $ Blind noopCommand
    VillagesTurn        -> arbitraryVoteLynchCommand game
    WerewolvesTurn      -> arbitraryVoteDevourCommand game
    WildChildsTurn      -> arbitraryChoosePlayerCommand game
    WitchsTurn          -> oneof
        [ arbitraryHealCommand game
        , arbitraryPassWitchsTurnCommand game
        , arbitraryPoisonCommand game
        ]
    WolfHoundsTurn      -> arbitraryChooseAllegianceCommand game

arbitraryChooseAllegianceCommand :: Game -> Gen (Blind Command)
arbitraryChooseAllegianceCommand game = do
    let wolfHoundsName  = game ^?! players . wolfHounds . name
    allegianceName      <- elements $ map (T.pack . show) [Villagers, Werewolves]

    return . Blind $ chooseAllegianceCommand wolfHoundsName allegianceName

arbitraryChoosePlayerCommand :: Game -> Gen (Blind Command)
arbitraryChoosePlayerCommand game = do
    let wildChild   = game ^?! players . wildChildren
    target          <- suchThat (arbitraryPlayer game) (wildChild /=)

    return . Blind $ choosePlayerCommand (wildChild ^. name) (target ^. name)

arbitraryChoosePlayersCommand :: Game -> Gen (Blind Command)
arbitraryChoosePlayersCommand game = do
    let scapegoatsName  = game ^?! players . scapegoats . name
    (NonEmpty players') <- NonEmpty <$> sublistOf (game ^.. players . traverse . alive)

    return . Blind $ choosePlayersCommand scapegoatsName (players' ^.. names)

arbitraryHealCommand :: Game -> Gen (Blind Command)
arbitraryHealCommand game = do
    let witchsName = game ^?! players . witches . name

    return . Blind $ if game ^. healUsed
        then noopCommand
        else healCommand witchsName

arbitraryPassDevotedServantsTurnCommand :: Game -> Gen (Blind Command)
arbitraryPassDevotedServantsTurnCommand game = do
    let devotedServantsName = game ^?! players . devotedServants . name

    return . Blind $ passDevotedServantsTurnCommand devotedServantsName

arbitraryPassWitchsTurnCommand :: Game -> Gen (Blind Command)
arbitraryPassWitchsTurnCommand game = do
    let witchsName = game ^?! players . witches . name

    return . Blind $ passCommand witchsName

arbitraryPoisonCommand :: Game -> Gen (Blind Command)
arbitraryPoisonCommand game = do
    let witchsName  = game ^?! players . witches . name
    target          <- arbitraryPlayer game

    return $ if isJust (game ^. poison)
        then Blind noopCommand
        else Blind $ poisonCommand witchsName (target ^. name)

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
    let applicableCallerNames = game ^.. players . traverse . alive . name

    if null applicableCallerNames
        then return $ Blind noopCommand
        else elements applicableCallerNames >>= \callersName ->
            return . Blind $ quitCommand callersName

arbitraryRevealCommand :: Game -> Gen (Blind Command)
arbitraryRevealCommand game = do
    let devotedServantsName = game ^?! players . devotedServants . name

    return . Blind $ revealCommand devotedServantsName

arbitrarySeeCommand :: Game -> Gen (Blind Command)
arbitrarySeeCommand game = do
    let seersName   = game ^?! players . seers . name
    target          <- arbitraryPlayer game

    return $ if isJust (game ^. see)
        then Blind noopCommand
        else Blind $ seeCommand seersName (target ^. name)

arbitraryVoteDevourCommand :: Game -> Gen (Blind Command)
arbitraryVoteDevourCommand game = do
    let applicableCallerNames   = getPendingVoters game ^.. werewolves . name
    target                      <- suchThat (arbitraryPlayer game) $ isn't werewolf

    if null applicableCallerNames
        then return $ Blind noopCommand
        else elements applicableCallerNames >>= \callerName ->
            return . Blind $ voteDevourCommand callerName (target ^. name)

arbitraryVoteLynchCommand :: Game -> Gen (Blind Command)
arbitraryVoteLynchCommand game = do
    let applicableCallerNames   = (game ^. allowedVoters) `intersect` (getPendingVoters game ^.. names)
    target                      <- arbitraryPlayer game

    if null applicableCallerNames
        then return $ Blind noopCommand
        else elements applicableCallerNames >>= \callerName ->
            return . Blind $ voteLynchCommand callerName (target ^. name)

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
