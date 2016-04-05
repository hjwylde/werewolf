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
    GameAtDevotedServantsTurn(..), GameAtGameOver(..), GameAtOrphansTurn(..),
    GameAtProtectorsTurn(..), GameAtScapegoatsTurn(..), GameAtSeersTurn(..), GameAtSunrise(..),
    GameAtVillagesTurn(..), GameAtWerewolvesTurn(..), GameAtWitchsTurn(..),
    GameAtWolfHoundsTurn(..),
    GameOnSecondRound(..),
    GameWithAllegianceChosen(..), GameWithAllowedVoters(..), GameWithConflictingVote(..),
    GameWithDeadPlayers(..), GameWithDevourEvent(..), GameWithDevourVotes(..), GameWithHeal(..),
    GameWithJesterRevealedAtVillagesTurn(..), GameWithLynchVotes(..), GameWithMajorityVote(..),
    GameWithNoAllowedVotersAtVillagesTurn(..), GameWithNoWerewolvesAtProtectorsTurn(..),
    GameWithOneAllegianceAlive(..), GameWithPassAtDevotedServantsTurn(..), GameWithPoison(..),
    GameWithProtect(..), GameWithProtectAndDevourVotes(..), GameWithRoleModel(..),
    GameWithRoleModelAtVillagesTurn(..), GameWithSee(..),

    -- ** Player
    arbitraryPlayerSet,

    -- * Contextual arbitraries

    -- ** Command
    arbitraryCommand, arbitraryOrphanChooseCommand, arbitraryWolfHoundChooseCommand,
    arbitraryScapegoatChooseCommand, arbitraryHealCommand, arbitraryDevotedServantPassCommand,
    arbitraryWitchPassCommand, arbitraryPoisonCommand, arbitraryProtectCommand,
    arbitraryQuitCommand, arbitraryRevealCommand, arbitrarySeeCommand, arbitraryWerewolfVoteCommand,
    arbitraryVillagerVoteCommand,
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
import Game.Werewolf.Command.DevotedServant as DevotedServant
import Game.Werewolf.Command.Global
import Game.Werewolf.Command.Hunter         as Hunter
import Game.Werewolf.Command.Orphan         as Orphan
import Game.Werewolf.Command.Protector
import Game.Werewolf.Command.Scapegoat      as Scapegoat
import Game.Werewolf.Command.Seer
import Game.Werewolf.Command.Villager       as Villager
import Game.Werewolf.Command.Werewolf       as Werewolf
import Game.Werewolf.Command.Witch          as Witch
import Game.Werewolf.Command.WolfHound      as WolfHound
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
    arbitrary = elements [Villagers, Werewolves]

instance Arbitrary Text where
    arbitrary = T.pack <$> vectorOf 6 (elements ['a'..'z'])

newtype NewGame = NewGame Game
    deriving (Eq, Show)

instance Arbitrary NewGame where
    arbitrary = NewGame . newGame <$> arbitraryPlayerSet

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

newtype GameAtProtectorsTurn = GameAtProtectorsTurn Game
    deriving (Eq, Show)

newtype GameAtOrphansTurn = GameAtOrphansTurn Game
    deriving (Eq, Show)

instance Arbitrary GameAtOrphansTurn where
    arbitrary = do
        game <- arbitrary

        return $ GameAtOrphansTurn (game & stage .~ OrphansTurn)

instance Arbitrary GameAtProtectorsTurn where
    arbitrary = do
        game <- arbitrary

        return $ GameAtProtectorsTurn (game & stage .~ ProtectorsTurn)

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
        (Blind command)             <- arbitraryWolfHoundChooseCommand game

        return $ GameWithAllegianceChosen (run_ (apply command) game)

newtype GameWithAllowedVoters = GameWithAllowedVoters Game
    deriving (Eq, Show)

instance Arbitrary GameWithAllowedVoters where
    arbitrary = do
        (GameAtScapegoatsTurn game) <- arbitrary
        (Blind command)             <- arbitraryScapegoatChooseCommand game

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

newtype GameWithJesterRevealed = GameWithJesterRevealed Game
    deriving (Eq, Show)

instance Arbitrary GameWithJesterRevealed where
    arbitrary = do
        game            <- arbitrary
        let jestersName = game ^?! players . jesters . name
        let game'       = game & jesterRevealed .~ True & allowedVoters %~ delete jestersName

        return $ GameWithJesterRevealed game'

newtype GameWithJesterRevealedAtVillagesTurn = GameWithJesterRevealedAtVillagesTurn Game
    deriving (Eq, Show)

instance Arbitrary GameWithJesterRevealedAtVillagesTurn where
    arbitrary = do
        (GameWithJesterRevealed game) <- arbitrary

        return $ GameWithJesterRevealedAtVillagesTurn (game & stage .~ VillagesTurn)

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

newtype GameWithNoAllowedVotersAtVillagesTurn = GameWithNoAllowedVotersAtVillagesTurn Game
    deriving (Eq, Show)

instance Arbitrary GameWithNoAllowedVotersAtVillagesTurn where
    arbitrary = do
        (GameAtVillagesTurn game) <- arbitrary

        return $ GameWithNoAllowedVotersAtVillagesTurn (game & allowedVoters .~ [])

newtype GameWithNoWerewolvesAtProtectorsTurn = GameWithNoWerewolvesAtProtectorsTurn Game
    deriving (Eq, Show)

instance Arbitrary GameWithNoWerewolvesAtProtectorsTurn where
    arbitrary = do
        (GameAtProtectorsTurn game) <- arbitrary
        let werewolfNames           = game ^.. players . werewolves . name
        let game'                   = foldr killPlayer game werewolfNames

        return $ GameWithNoWerewolvesAtProtectorsTurn game'

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
        (Blind command)                     <- arbitraryDevotedServantPassCommand game

        return $ GameWithPassAtDevotedServantsTurn (run_ (apply command) game)

newtype GameWithPassAtWitchsTurn = GameWithPassAtWitchsTurn Game
    deriving (Eq, Show)

instance Arbitrary GameWithPassAtWitchsTurn where
    arbitrary = do
        game            <- arbitrary
        let game'       = game & stage .~ WitchsTurn
        (Blind command) <- arbitraryWitchPassCommand game'

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
        let game'       = game & stage .~ ProtectorsTurn
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
        (GameAtOrphansTurn game) <- arbitrary
        (Blind command)             <- arbitraryOrphanChooseCommand game
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

arbitraryPlayerSet :: Gen [Player]
arbitraryPlayerSet = do
    n       <- choose (14, 30)
    players <- nubOn (view name) <$> infiniteList

    let playersWithRestrictedRole = map (\role' -> players ^?! traverse . filteredBy role role') restrictedRoles

    let simpleWerewolves'   = players ^.. taking (n `quot` 5 + 1) simpleWerewolves
    let simpleVillagers'    = players ^.. taking (n - length restrictedRoles - length simpleWerewolves') simpleVillagers

    return $ playersWithRestrictedRole ++ simpleVillagers' ++ simpleWerewolves'

arbitraryCommand :: Game -> Gen (Blind Command)
arbitraryCommand game = case game ^. stage of
    DevotedServantsTurn -> oneof
        [ arbitraryDevotedServantPassCommand game
        , arbitraryRevealCommand game
        ]
    FerinasGrunt        -> return $ Blind noopCommand
    GameOver            -> return $ Blind noopCommand
    HuntersTurn1        -> arbitraryHunterChooseCommand game
    HuntersTurn2        -> arbitraryHunterChooseCommand game
    Lynching            -> return $ Blind noopCommand
    OrphansTurn         -> arbitraryOrphanChooseCommand game
    ProtectorsTurn      -> arbitraryProtectCommand game
    ScapegoatsTurn      -> arbitraryScapegoatChooseCommand game
    SeersTurn           -> arbitrarySeeCommand game
    Sunrise             -> return $ Blind noopCommand
    Sunset              -> return $ Blind noopCommand
    VillagesTurn        -> arbitraryVillagerVoteCommand game
    WerewolvesTurn      -> arbitraryWerewolfVoteCommand game
    WitchsTurn          -> oneof
        [ arbitraryHealCommand game
        , arbitraryWitchPassCommand game
        , arbitraryPoisonCommand game
        ]
    WolfHoundsTurn      -> arbitraryWolfHoundChooseCommand game

arbitraryHunterChooseCommand :: Game -> Gen (Blind Command)
arbitraryHunterChooseCommand game = do
    let hunter  = game ^?! players . hunters
    target      <- arbitraryPlayer game

    return . Blind $ Hunter.chooseCommand (hunter ^. name) (target ^. name)

arbitraryOrphanChooseCommand :: Game -> Gen (Blind Command)
arbitraryOrphanChooseCommand game = do
    let orphan  = game ^?! players . orphans
    target      <- suchThat (arbitraryPlayer game) (orphan /=)

    return . Blind $ Orphan.chooseCommand (orphan ^. name) (target ^. name)

arbitraryWolfHoundChooseCommand :: Game -> Gen (Blind Command)
arbitraryWolfHoundChooseCommand game = do
    let wolfHoundsName  = game ^?! players . wolfHounds . name
    allegianceName      <- elements $ map (T.pack . show) [Villagers, Werewolves]

    return . Blind $ WolfHound.chooseCommand wolfHoundsName allegianceName

arbitraryScapegoatChooseCommand :: Game -> Gen (Blind Command)
arbitraryScapegoatChooseCommand game = do
    let scapegoatsName  = game ^?! players . scapegoats . name
    (NonEmpty players') <- NonEmpty <$> sublistOf (game ^.. players . traverse . alive)

    return . Blind $ Scapegoat.chooseCommand scapegoatsName (players' ^.. names)

arbitraryHealCommand :: Game -> Gen (Blind Command)
arbitraryHealCommand game = do
    let witchsName = game ^?! players . witches . name

    return . Blind $ if game ^. healUsed
        then noopCommand
        else healCommand witchsName

arbitraryDevotedServantPassCommand :: Game -> Gen (Blind Command)
arbitraryDevotedServantPassCommand game = do
    let devotedServantsName = game ^?! players . devotedServants . name

    return . Blind $ DevotedServant.passCommand devotedServantsName

arbitraryWitchPassCommand :: Game -> Gen (Blind Command)
arbitraryWitchPassCommand game = do
    let witchsName = game ^?! players . witches . name

    return . Blind $ Witch.passCommand witchsName

arbitraryPoisonCommand :: Game -> Gen (Blind Command)
arbitraryPoisonCommand game = do
    let witchsName  = game ^?! players . witches . name
    target          <- arbitraryPlayer game

    return $ if isJust (game ^. poison)
        then Blind noopCommand
        else Blind $ poisonCommand witchsName (target ^. name)

arbitraryProtectCommand :: Game -> Gen (Blind Command)
arbitraryProtectCommand game = do
    let protector    = game ^?! players . protectors
    -- TODO (hjw): add suchThat (/= priorProtect)
    target          <- suchThat (arbitraryPlayer game) (protector /=)

    return $ if isJust (game ^. protect)
        then Blind noopCommand
        else Blind $ protectCommand (protector ^. name) (target ^. name)

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

arbitraryWerewolfVoteCommand :: Game -> Gen (Blind Command)
arbitraryWerewolfVoteCommand game = do
    let applicableCallerNames   = getPendingVoters game ^.. werewolves . name
    target                      <- suchThat (arbitraryPlayer game) $ isn't werewolf

    if null applicableCallerNames
        then return $ Blind noopCommand
        else elements applicableCallerNames >>= \callerName ->
            return . Blind $ Werewolf.voteCommand callerName (target ^. name)

arbitraryVillagerVoteCommand :: Game -> Gen (Blind Command)
arbitraryVillagerVoteCommand game = do
    let applicableCallerNames   = (game ^. allowedVoters) `intersect` (getPendingVoters game ^.. names)
    target                      <- arbitraryPlayer game

    if null applicableCallerNames
        then return $ Blind noopCommand
        else elements applicableCallerNames >>= \callerName ->
            return . Blind $ Villager.voteCommand callerName (target ^. name)

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
