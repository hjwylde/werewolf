{-|
Module      : Game.Werewolf.Role
Description : Simplistic role data structure and instances.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

Roles are split into four categories:

* The Ambiguous.
* The Loners.
* The Villagers.
* The Werewolves.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Werewolf.Role (
    -- * Role
    Role,
    name, allegiance, balance, description, rules,

    Allegiance(..),
    _FallenAngel, _Villagers, _Werewolves,

    -- ** Instances
    allRoles, restrictedRoles,

    -- *** The Ambiguous
    -- | No-one knows the true nature of the Ambiguous, sometimes not even the Ambiguous themselves!
    --
    --   The Ambiguous are able to change allegiance throughout the game.
    orphanRole, wolfHoundRole,

    -- *** The Loners
    -- | The Loners look out for themselves and themselves alone.

    --   The Loners must complete their own objective.
    fallenAngelRole,

    -- *** The Villagers
    -- | Fraught with fear of the unseen enemy, the Villagers must work together to determine the
    --   truth and eliminate the threat to Fougères. The task before them will not be easy, but a
    --   certain few have learnt some tricks over the years that may turn out rather useful.

    --   The Villagers must lynch all of the Werewolves.
    druidRole, hunterRole, jesterRole, protectorRole, scapegoatRole, seerRole, simpleVillagerRole,
    villagerVillagerRole, witchRole,

    -- *** The Werewolves
    -- | Hiding in plain sight, the Werewolves are not a small trifle.

    --   The Werewolves must devour all of the Villagers.
    simpleWerewolfRole,

    -- * Utility functions
    is, isn't, filteredBy,
) where

import Control.Lens hiding (isn't)

import           Data.Function
import           Data.List
import           Data.Monoid
import           Data.Text     (Text)
import qualified Data.Text     as T

-- | Role definitions require only a few pieces of information.
--   Most of the game logic behind a role is implemented in "Game.Werewolf.Command" and
--   "Game.Werewolf.Engine".
--
--   The @balance@ attribute on a role indicates the allegiance it favours. For example, a Simple
--   Werewolf has a balance of -4 while the Seer has a balance of 2. A balance of 0 means it favours
--   neither allegiance.
--
--   N.B., role equality is defined on just the 'name' as a role's 'allegiance' may change
--   throughout the game.
data Role = Role
    { _name        :: Text
    , _allegiance  :: Allegiance
    , _balance     :: Int
    , _description :: Text
    , _rules       :: Text
    } deriving (Read, Show)

-- | The Loner allegiances are seldom used, rather they are present for correctness.
data Allegiance = FallenAngel | Villagers | Werewolves
    deriving (Eq, Read, Show)

makeLenses ''Role

instance Eq Role where
    (==) = (==) `on` view name

makePrisms ''Allegiance

-- | A list containing all the roles defined in this file.
allRoles :: [Role]
allRoles =
    [ druidRole
    , fallenAngelRole
    , hunterRole
    , jesterRole
    , orphanRole
    , protectorRole
    , scapegoatRole
    , seerRole
    , simpleVillagerRole
    , simpleWerewolfRole
    , villagerVillagerRole
    , witchRole
    , wolfHoundRole
    ]

-- | A list containing roles that are restricted to a single instance per 'Game'.
--
--   @
--   'restrictedRoles' = 'allRoles' \\\\ ['simpleVillagerRole', 'simpleWerewolfRole']
--   @
restrictedRoles :: [Role]
restrictedRoles = allRoles \\ [simpleVillagerRole, simpleWerewolfRole]

-- | /Abandoned by their parents as a child, with no-one wanting to look after another mouth to/
--   /feed, the Orphan was left to fend for themself. No-one looks twice at the Orphan and even/
--   /fewer offer kindness towards the lonely child. One day however, one townsperson changes all/
--   /this. He offers the Orphan food, water and a roof over his head. Grateful for his chairty and/
--   /affection, the Orphan makes him their role model. Pray that no ill should befall their role/
--   /model, for who knows in such an event whom, or what, the Orphan may turn to.../
--
--   On the first night, the Orphan may choose a player to become his role model. If during the game
--   the role model is eliminated, the Orphan becomes a Werewolf. He will then wake up the next
--   night with his peers and will devour with them each night until the end of the game. However
--   for as long as the Orphan's role model is alive, he remains a Villager.
orphanRole :: Role
orphanRole = Role
    { _name         = "Orphan"
    , _allegiance   = Villagers
    , _balance      = -1
    , _description  = T.unwords
        [ "Abandoned by their parents as a child, with no-one wanting to look after another mouth"
        , "to feed, the Orphan was left to fend for themself. No-one looks twice at the Orphan and"
        , "even fewer offer kindness towards the lonely child. One day however, one townsperson"
        , "changes all this. He offers the Orphan food, water and a roof over his head. Grateful"
        , "for his chairty and affection, the Orphan makes him their role model. Pray that no ill"
        , "should befall their role model, for who knows in such an event whom, or what, the Orphan"
        , "may turn to..."
        ]
    , _rules        = T.unwords
        [ "On the first night, the Orphan may choose a player to become his role model. If during"
        , "the game the role model is eliminated, the Orphan becomes a Werewolf. He will then wake"
        , "up the next night with his peers and will devour with them each night until the end of"
        , "the game. However for as long as the Orphan's role model is alive, he remains a"
        , "Villager."
        ]
    }

-- | /All dogs know in the depths of their soul that their ancestors were wolves and that it's/
--   /mankind who has kept them in the state of childishness and fear, the faithful and generous/
--   /companions. In any case, only the Wolf-hound can decide if he'll obey his human and civilized/
--   /master or if he'll listen to the call of wild nature buried within him./
--
--   On the first night, the Wolf-hound chooses if he wants to be a Simple Villager or Werewolf. The
--   choice is final.
wolfHoundRole :: Role
wolfHoundRole = Role
    { _name         = "Wolf-hound"
    , _allegiance   = Villagers
    , _balance      = -2
    , _description  = T.unwords
        [ "All dogs know in the depths of their soul that their ancestors were wolves and that it's"
        , "mankind who has kept them in the state of childishness and fear, the faithful and"
        , "generous companions. In any case, only the Wolf-hound can decide if he'll obey his human"
        , "and civilized master or if he'll listen to the call of wild nature buried within him."
        ]
    , _rules        = T.unwords
        [ "On the first night, the Wolf-hound chooses if he wants to be a Simple Villager or"
        , "Werewolf. The choice is final."
        ]
    }

-- | /Long ago during the War in Heaven, angels fell from the sky as one by one those that followed/
--   /Lucifer were defeated. The Fallen Angel was one such being and is now one of the few angels/
--   /left on Earth. For centuries they have lived amongst mortal Villagers as punishment for their/
--   /sins and wrongdoings. Nothing is worse punishment for them, the Fallen Angel yearns for death/
--   /to once again be free!
--
--   When the Fallen Angel is in play, the game always begins with the village's vote and then the
--   first night.
--
--   The Fallen Angel wins if they manage to get eliminated on the first round (day or night). If
--   they fail, then they become a Simple Villager for the rest of the game.
fallenAngelRole :: Role
fallenAngelRole = Role
    { _name         = "Fallen Angel"
    , _allegiance   = FallenAngel
    , _balance      = 0
    , _description  = T.unwords
        [ "Long ago during the War in Heaven, angels fell from the sky as one by one those that"
        , "followed Lucifer were defeated. The Fallen Angel was one such being and is now one of"
        , "the few angels left on Earth. For centuries they have lived amongst mortal Villagers as"
        , "punishment for their sins and wrongdoings. Nothing is worse punishment for them, the"
        , "Fallen Angel yearns for death to once again be free!"
        ]
    , _rules        = T.intercalate "\n"
        [ T.unwords
            [ "When the Fallen Angel is in play, the game always begins with the village's vote and"
            , "then the first night."
            ]
        , T.unwords
            [ "The Fallen Angel wins if they manage to get eliminated on the first round (day or"
            , "night). If they fail, then they become a Simple Villager for the rest of the game."
            ]
        ]
    }

-- | /How honoured we are to be in the presence of such a noble leader. The return of the Druid/
--   /marks an exceptional time in Fougères's history! Friend of the woodland creatures, practiced/
--   /philosopher and now, with the help of Ferina their companion, a bane to the Werewolves/
--   /themselves! My does she have a nose on her, strong enough to sniff out lycanthropes in close/
--   /proximity! Listen for her grunt and heed the Druid's words for they will not let you down./
--
--   Each morning when Ferina wakes from her slumber she will be alert and cautious. If the Druid is
--   next to a Werewolf then Ferina will grunt in warning.
druidRole :: Role
druidRole = Role
    { _name         = "Druid"
    , _allegiance   = Villagers
    , _balance      = 3
    , _description  = T.unwords
        [ "How honoured we are to be in the presence of such a noble leader. The return of the"
        , "Druid marks an exceptional time in Fougères's history! Friend of the woodland creatures,"
        , "practiced philosopher and now, with the help of Ferina their companion, a bane to the"
        , "Werewolves themselves! My does she have a nose on her, strong enough to sniff out"
        , "lycanthropes in close proximity! Listen for her grunt and heed the Druid's words for"
        , "they will not let you down."
        ]
    , _rules        = T.unwords
        [ "Each morning when Ferina wakes from her slumber she will be alert and cautious. If the"
        , "Druid is next to a Werewolf then Ferina will grunt in warning."
        ]
    }

-- | /A skilled marksman with quick reflexes. In the unfortunate situation that they are jumped and/
--   /killed unjustly, they are able to let off a shot at their attacker, killing them instantly./
--   /The Hunter never misses./
--
--   If the Hunter gets killed they get to choose one player, believed to be an attacker, to kill
--   immediately.
hunterRole :: Role
hunterRole = Role
    { _name         = "Hunter"
    , _allegiance   = Villagers
    , _balance      = 2
    , _description  = T.unwords
        [ "A skilled marksman with quick reflexes. In the unfortunate situation that they are"
        , "jumped and killed unjustly, they are able to let off a shot at their attacker, killing"
        , "them instantly. The Hunter never misses."
        ]
    , _rules        = T.unwords
        [ "If the Hunter gets killed they get to choose one player, believed to be an attacker, to"
        , "kill immediately."
        ]
    }

-- | /The Protector is one of the few pure of heart and altruistic Villagers. They are forever/
--   /putting others needs above their own, standing guard at night against this terrifying foe./
--   /Each night they fight against the Werewolves with naught but a sword and shield, potentially/
--   /saving an innocents life./
--
--   Each night the Protector may choose a player deemed worthy of their protection. That player is
--   safe for that night night (and only that night) against the Werewolves.
--
--   The Protector may not protect the same player two nights in a row.
protectorRole :: Role
protectorRole = Role
    { _name         = "Protector"
    , _allegiance   = Villagers
    , _balance      = 2
    , _description  = T.unwords
        [ "The Protector is one of the few pure of heart and altruistic Villagers. They are forever"
        , "putting others needs above their own, standing guard at night against this terrifying"
        , "foe. Each night they fight against the Werewolves with naught but a sword and shield,"
        , "potentially saving an innocents life."
        ]
    , _rules        = T.intercalate "\n"
        [ T.unwords
            [ "Each night the Protector may choose a player deemed worthy of their protection. That"
            , "player is safe for that night night (and only that night) against the Werewolves."
            ]
        , "The Protector may not protect the same player two nights in a row."
        ]
    }

-- | /Werewolves don't just spring up out of the ground! That's where dwarves come from. Clearly/
--   /someone is to blame for this affliction to Fougères. Unluckily for the Scapegoat, since/
--   /no-one actually knows who brought them here, the blame is always laid upon them!/
--
--   If the village's vote ends in a tie, it's the Scapegoat who is eliminated instead of no-one.
--
--   In this event, the Scapegoat has one last task to complete: he must choose whom is permitted to
--   vote or not on the next day.
scapegoatRole :: Role
scapegoatRole = Role
    { _name         = "Scapegoat"
    , _allegiance   = Villagers
    , _balance      = 0
    , _description  = T.unwords
        [ "Werewolves don't just spring up out of the ground! That's where dwarves come from."
        , "Clearly someone is to blame for this affliction to Fougères. Unluckily for the"
        , "Scapegoat, since no-one actually knows who brought them here, the blame is always laid"
        , "upon them!"
        ]
    , _rules        = T.intercalate "\n"
        [ T.unwords
            [ "If the village's vote ends in a tie, it's the Scapegoat who is eliminated instead of"
            , "no-one."
            ]
        , T.unwords
            [ "In this event, the Scapegoat has one last task to complete: he must choose whom is"
            , "permitted to vote or not on the next day."
            ]
        ]
    }

-- | /Frequently misunderstood and thought to be a fortune teller, the Seer has the ability to see/
--   /into fellow townsfolk and determine their true nature. This ability to see is not given out/
--   /lightly, for certain it is a gift! Visions will always be true, but only for the present as/
--   /not even the Seer knowns what the future holds./
--
--   Each night the Seer sees the allegiance of a player of their choice.
seerRole :: Role
seerRole = Role
    { _name         = "Seer"
    , _allegiance   = Villagers
    , _balance      = 2
    , _description  = T.unwords
        [ "Frequently misunderstood and thought to be a fortune teller, the Seer has the ability to"
        , "see into fellow townsfolk and determine their true nature. This ability to see is not"
        , "given out lightly, for certain it is a gift! Visions will always be true, but only for"
        , "the present as not even the Seer knowns what the future holds."
        ]
    , _rules        = "Each night the Seer sees the allegiance of a player of their choice."
    }

-- | /A simple, ordinary townsperson in every way. Some may be cobblers, others bakers or even/
--   /nobles. No matter their differences though, the plight of Werewolves in Fougères unites them/
--   /in this unfortunate time./
--
--   A Simple Villager has no special abilities, they must use their guile to determine whom among
--   them is not who they say they are.
simpleVillagerRole :: Role
simpleVillagerRole = Role
    { _name         = "Simple Villager"
    , _allegiance   = Villagers
    , _balance      = 1
    , _description  = T.unwords
        [ "A simple, ordinary townsperson in every way. Some may be cobblers, others bakers or even"
        , "nobles. No matter their differences though, the plight of Werewolves in Fougères unites"
        , "them in this unfortunate time."
        ]
    , _rules        = T.unwords
        [ "A Simple Villager has no special abilities, they must use their guile to determine whom"
        , "among them is not who they say they are."
        ]
    }

-- | /Every village needs a Jester, they're so stupid but provide so much entertainment! The Jester/
--   /may not have any special abilities, but at least no one in the village would want to hurt/
--   /him./
--
--   If the village votes to lynch the Jester, his identity is revealed. The village realise there's
--   no point in burning him and so he is set free.
--
--   The Jester continues to play but may no longer vote as no one can take him seriously.
jesterRole :: Role
jesterRole = Role
    { _name         = "Jester"
    , _allegiance   = Villagers
    , _balance      = 0
    , _description  = T.unwords
        [ "Every village needs a Jester, they're so stupid but provide so much entertainment! The"
        , "Jester may not have any special abilities, but at least no one in the village would want"
        , "to hurt him."
        ]
    , _rules        = T.intercalate "\n"
        [ T.unwords
            [ "If the village votes to lynch the Jester, his identity is revealed. The village"
            , "realise there's no point in burning him and so he is set free."
            ]
        , T.unwords
            [ "The Jester continues to play but may no longer vote as no one can take him"
            , "seriously."
            ]
        ]
    }

-- | /This person has a soul as clear and transparent as the water from a mountain stream. They/
--   /will deserve the attentive ear of their peers and will make their word decisive in crucial/
--   /moments./
--
--   When the game begins, the village is told the identity of the Villager-Villager, thus ensuring
--   certainty that its owner is truly an innocent Villager.
villagerVillagerRole :: Role
villagerVillagerRole = Role
    { _name         = "Villager-Villager"
    , _allegiance   = Villagers
    , _balance      = 2
    , _description  = T.unwords
        [ "This person has a soul as clear and transparent as the water from a mountain stream."
        , "They will deserve the attentive ear of their peers and will make their word decisive in"
        , "crucial moments."
        ]
    , _rules        = T.unwords
        [ "When the game begins, the village is told the identity of the Villager-Villager, thus"
        , "ensuring certainty that its owner is truly an innocent Villager."
        ]
    }

-- | /Somehow forgotten with the coming of the Werewolves, the Witch has a chance to prove themself/
--   /valuable to the village. The Witch is blessed (or maybe cursed) with the ability to make two/
--   /powerful potions; one of which may heal a victim of the Werewolves, the other able to poison/
--   /a player. The use of these potions could alter the village's currently misguided perception/
--   /of the Witch./
--
--   The Witch is called after the Werewolves. They are able to heal and poison one player per game.
--   There is no restriction on using both potions in one night or to heal themself.
witchRole :: Role
witchRole = Role
    { _name         = "Witch"
    , _allegiance   = Villagers
    , _balance      = 3
    , _description  = T.unwords
        [ "Somehow forgotten with the coming of the Werewolves, the Witch has a chance to prove"
        , "themself valuable to the village. The Witch is blessed (or maybe cursed) with the"
        , "ability to make two powerful potions; one of which may heal a victim of the Werewolves,"
        , "the other able to poison a player. The use of these potions could alter the village's"
        , "currently misguided perception of the Witch."
        ]
    , _rules        = T.unwords
        [ "The Witch is called after the Werewolves. They are able to heal and poison one player"
        , "per game. There is no restriction on using both potions in one night or to heal"
        , "themself."
        ]
    }

-- | /It's difficult to tell, but a Werewolf is merely a Villager underneath; unfortunately one/
--   /that has been afflicted with lycanthropy and consequently transforms each night into a/
--   /fearsome lupine. Not all hope is yet lost, some Villagers would like to try and cure the/
--   /curse of lycanthropy. Perhaps one day this dream will be realised./
--
--   A Werewolf may never devour another Werewolf.
simpleWerewolfRole :: Role
simpleWerewolfRole = Role
    { _name         = "Simple Werewolf"
    , _allegiance   = Werewolves
    , _balance      = -4
    , _description  = T.unwords
        [ "It's difficult to tell, but a Werewolf is merely a Villager underneath; unfortunately"
        , "one that has been afflicted with lycanthropy and consequently transforms each night into"
        , "a fearsome lupine. Not all hope is yet lost, some Villagers would like to try and cure"
        , "the curse of lycanthropy. Perhaps one day this dream will be realised."
        ]
    , _rules        = "A Werewolf may never devour another Werewolf."
    }

-- | The counter-part to 'isn't', but more general as it takes a 'Getting' instead.
is :: Getting Any s a -> s -> Bool
is query = has query

-- | A re-write of 'Control.Lens.Prism.isn't' to be more general by taking a 'Getting' instead.
isn't :: Getting All s a -> s -> Bool
isn't query = hasn't query

-- | A companion to 'filtered' that, rather than using a predicate, filters on the given lens for
-- matches.
filteredBy :: Eq b => Lens' a b -> b -> Traversal' a a
filteredBy lens value = filtered ((value ==) . view lens)
