{-|
Module      : Game.Werewolf.Game
Description : Game and stage data structures.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Game and stage data structures.
-}

{-# LANGUAGE TemplateHaskell #-}

module Game.Werewolf.Game (
    -- * Game
    Game, stage, round, players, events, passes, allowedVoters, heal, healUsed, poison, poisonUsed,
    priorProtect, protect, roleModel, scapegoatBlamed, see, villageIdiotRevealed, votes,
    newGame,

    -- ** Manipulations
    killPlayer, setPlayerRole, setPlayerAllegiance,

    -- ** Queries
    isFirstRound,
    doesPlayerExist,
    getPassers, getPlayerVote, getAllowedVoters, getPendingVoters, getVoteResult,

    -- * Stage
    Stage(..),
    allStages,
    stageCycle, stageAvailable,

    -- ** Queries
    isDefendersTurn, isGameOver, isScapegoatsTurn, isSeersTurn, isSunrise, isSunset, isVillagesTurn,
    isWerewolvesTurn, isWildChildsTurn, isWitchsTurn, isWolfHoundsTurn,

    -- * Event
    Event(..),

    -- ** Queries
    getDevourEvent
) where

import Control.Lens

import           Data.List.Extra
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Text       (Text)

import Game.Werewolf.Internal.Player
import Game.Werewolf.Internal.Role   hiding (name)

import Prelude hiding (round)

data Game = Game
    { _stage                :: Stage
    , _round                :: Int
    , _players              :: [Player]
    , _events               :: [Event]
    , _passes               :: [Text]
    , _allowedVoters        :: [Text]
    , _heal                 :: Bool
    , _healUsed             :: Bool
    , _poison               :: Maybe Text
    , _poisonUsed           :: Bool
    , _priorProtect         :: Maybe Text
    , _protect              :: Maybe Text
    , _roleModel            :: Maybe Text
    , _scapegoatBlamed      :: Bool
    , _see                  :: Maybe Text
    , _villageIdiotRevealed :: Bool
    , _votes                :: Map Text Text
    } deriving (Eq, Read, Show)

data Stage  = GameOver | DefendersTurn | ScapegoatsTurn | SeersTurn | Sunrise | Sunset
            | VillagesTurn | WerewolvesTurn | WildChildsTurn | WitchsTurn | WolfHoundsTurn
    deriving (Eq, Read, Show)

data Event = DevourEvent Text | NoDevourEvent | PoisonEvent Text
    deriving (Eq, Read, Show)

makeLenses ''Game

makeLenses ''Stage

newGame :: [Player] -> Game
newGame players = game & stage .~ head (filter (stageAvailable game) stageCycle)
    where
        game = Game
            { _stage                = Sunset
            , _round                = 0
            , _players              = players
            , _events               = []
            , _passes               = []
            , _allowedVoters        = map (view name) players
            , _heal                 = False
            , _healUsed             = False
            , _poison               = Nothing
            , _poisonUsed           = False
            , _priorProtect         = Nothing
            , _protect              = Nothing
            , _roleModel            = Nothing
            , _scapegoatBlamed      = False
            , _see                  = Nothing
            , _villageIdiotRevealed = False
            , _votes                = Map.empty
            }

killPlayer :: Text -> Game -> Game
killPlayer name' game = game & players %~ map (\player -> if player ^. name == name' then player & state .~ Dead else player)

setPlayerRole :: Text -> Role -> Game -> Game
setPlayerRole name' role' game = game & players %~ map (\player -> if player ^. name == name' then player & role .~ role' else player)

setPlayerAllegiance :: Text -> Allegiance -> Game -> Game
setPlayerAllegiance name' allegiance' game = game & players %~ map (\player -> if player ^. name == name' then player & role . allegiance .~ allegiance' else player)

isFirstRound :: Game -> Bool
isFirstRound game = game ^. round == 0

doesPlayerExist :: Text -> Game -> Bool
doesPlayerExist name = isJust . findByName name . view players

getPassers :: Game -> [Player]
getPassers game = map (`findByName_` players') passes'
    where
        players'    = game ^. players
        passes'     = game ^. passes

getPlayerVote :: Text -> Game -> Maybe Text
getPlayerVote playerName game = game ^. votes . at playerName

getAllowedVoters :: Game -> [Player]
getAllowedVoters game = map (`findByName_` players') (game ^. allowedVoters)
    where
        players' = game ^. players

getPendingVoters :: Game -> [Player]
getPendingVoters game = filter (flip Map.notMember votes' . view name) alivePlayers
    where
        votes'          = game ^. votes
        alivePlayers    = filterAlive $ game ^. players

getVoteResult :: Game -> [Player]
getVoteResult game = map (`findByName_` players') result
    where
        players'    = game ^. players
        votees      = Map.elems $ game ^. votes
        result      = last $ groupSortOn (\votee -> length $ elemIndices votee votees) (nub votees)

allStages :: [Stage]
allStages =
    [ VillagesTurn
    , ScapegoatsTurn
    , Sunset
    , SeersTurn
    , WildChildsTurn
    , DefendersTurn
    , WolfHoundsTurn
    , WerewolvesTurn
    , WitchsTurn
    , Sunrise
    , GameOver
    ]

stageCycle :: [Stage]
stageCycle = cycle $ allStages \\ [GameOver]

stageAvailable :: Game -> Stage -> Bool
stageAvailable game DefendersTurn   = any isDefender (filterAlive $ game ^. players)
stageAvailable _ GameOver           = False
stageAvailable game ScapegoatsTurn  = game ^. scapegoatBlamed
stageAvailable game SeersTurn       = any isSeer (filterAlive $ game ^. players)
stageAvailable _ Sunrise            = True
stageAvailable _ Sunset             = True
stageAvailable game VillagesTurn    =
    (any isAngel (filterAlive $ game ^. players)
    || not (isFirstRound game))
    && any isAlive (getAllowedVoters game)
stageAvailable game WerewolvesTurn  = any isWerewolf (filterAlive $ game ^. players)
stageAvailable game WildChildsTurn  =
    any isWildChild (filterAlive $ game ^. players)
    && isNothing (game ^. roleModel)
stageAvailable game WitchsTurn      =
    any isWitch (filterAlive $ game ^. players)
    && (not (game ^. healUsed) || not (game ^. poisonUsed))
stageAvailable game WolfHoundsTurn  = any isWolfHound (filterAlive $ game ^. players)

isDefendersTurn :: Game -> Bool
isDefendersTurn game = game ^. stage == DefendersTurn

isGameOver :: Game -> Bool
isGameOver game = game ^. stage == GameOver

isScapegoatsTurn :: Game -> Bool
isScapegoatsTurn game = game ^. stage == ScapegoatsTurn

isSeersTurn :: Game -> Bool
isSeersTurn game = game ^. stage == SeersTurn

isSunrise :: Game -> Bool
isSunrise game = game ^. stage == Sunrise

isSunset :: Game -> Bool
isSunset game = game ^. stage == Sunset

isVillagesTurn :: Game -> Bool
isVillagesTurn game = game ^. stage == VillagesTurn

isWerewolvesTurn :: Game -> Bool
isWerewolvesTurn game = game ^. stage == WerewolvesTurn

isWildChildsTurn :: Game -> Bool
isWildChildsTurn game = game ^. stage == WildChildsTurn

isWitchsTurn :: Game -> Bool
isWitchsTurn game = game ^. stage == WitchsTurn

isWolfHoundsTurn :: Game -> Bool
isWolfHoundsTurn game = game ^. stage == WolfHoundsTurn

getDevourEvent :: Game -> Maybe Event
getDevourEvent game = listToMaybe [event | event@(DevourEvent _) <- game ^. events]
