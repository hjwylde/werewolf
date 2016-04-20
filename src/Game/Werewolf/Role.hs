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
    _NoOne, _Villagers, _Werewolves,

    -- ** Instances
    allRoles, restrictedRoles,

    -- *** The Ambiguous
    -- | No-one knows the true nature of the Ambiguous, sometimes not even the Ambiguous themselves!
    --
    --   The Ambiguous are able to change allegiance throughout the game.
    orphanRole, villageDrunkRole,

    -- *** The Loners
    -- | The Loners look out for themselves and themselves alone.

    --   The Loners must complete their own objective.
    fallenAngelRole,

    -- *** The Villagers
    -- | Fraught with fear of the unseen enemy, the Villagers must work together to determine the
    --   truth and eliminate the threat to Fougères. The task before them will not be easy, but a
    --   certain few have learnt some tricks over the years that may turn out rather useful.

    --   The Villagers must lynch all of the Werewolves.
    beholderRole, crookedSenatorRole, druidRole, hunterRole, jesterRole, lycanRole, protectorRole,
    scapegoatRole, seerRole, simpleVillagerRole, trueVillagerRole, witchRole,

    -- *** The Werewolves
    -- | Hiding in plain sight, the Werewolves are not a small trifle.

    --   The Werewolves must devour all of the Villagers.
    alphaWolfRole, simpleWerewolfRole,
) where

import Control.Lens

import           Data.Function
import           Data.List
import           Data.String
import           Data.String.Humanise
import           Data.Text            (Text)
import qualified Data.Text            as T

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

-- | The 'NoOne' allegiance is used for the Loners. It is not used to determine who has won (i.e.,
--   if one Loner wins, the others still lose).
data Allegiance = NoOne | Villagers | Werewolves
    deriving (Eq, Read, Show)

instance Humanise Allegiance where
    humanise NoOne      = fromString "no-one"
    humanise Villagers  = fromString "Villagers"
    humanise Werewolves = fromString "Werewolves"

makeLenses ''Role

instance Eq Role where
    (==) = (==) `on` view name

makePrisms ''Allegiance

-- | A list containing all the roles defined in this file.
allRoles :: [Role]
allRoles =
    [ alphaWolfRole
    , beholderRole
    , crookedSenatorRole
    , druidRole
    , fallenAngelRole
    , hunterRole
    , jesterRole
    , lycanRole
    , orphanRole
    , protectorRole
    , scapegoatRole
    , seerRole
    , simpleVillagerRole
    , simpleWerewolfRole
    , trueVillagerRole
    , villageDrunkRole
    , witchRole
    ]

-- | A list containing roles that are restricted to a single instance per 'Game'.
--
--   @
--   'restrictedRoles' = 'allRoles' \\\\ ['simpleVillagerRole', 'simpleWerewolfRole']
--   @
restrictedRoles :: [Role]
restrictedRoles = allRoles \\ [simpleVillagerRole, simpleWerewolfRole]

-- | /Abandoned by their parents as a child, with no-one wanting to look after another mouth to/
--   /feed, the Orphan was left to fend for themself. No-one looked twice at the Orphan and even/
--   /fewer showed kindness towards the lonely child. One day however, one townsperson changed all/
--   /this. He offered the Orphan food, water and a roof over their head. Grateful for his chairty/
--   /and affection, the Orphan made him their role model. Pray that no ill should befall their/
--   /role model, for they are the only one conforming the Orphan as a Villager./
--
--   On the first night, the Orphan chooses a player to become their role model. So long as the role
--   model is alive, the Orphan is a Villager. If however the role model is eliminated, then the
--   Orphan becomes a Werewolf.
orphanRole :: Role
orphanRole = Role
    { _name         = "Orphan"
    , _allegiance   = Villagers
    , _balance      = -1
    , _description  = T.unwords
        [ "Abandoned by their parents as a child, with no-one wanting to look after another mouth"
        , "to feed, the Orphan was left to fend for themself. No-one looked twice at the Orphan and"
        , "even fewer showed kindness towards the lonely child. One day however, one townsperson"
        , "changed all this. He offered the Orphan food, water and a roof over their head. Grateful"
        , "for his chairty and affection, the Orphan made him their role model. Pray that no ill"
        , "should befall their role model, for they are the only one conforming the Orphan as a"
        , "Villager."
        ]
    , _rules        = T.unwords
        [ "On the first night, the Orphan chooses a player to become their role model. So long as"
        , "the role model is alive, the Orphan is a Villager. If however the role model is"
        , "eliminated, then the Orphan becomes a Werewolf."
        ]
    }

-- | /Hah, maybe not as liked as the Jester, but the Drunk sure does their fair share of stupid/
--   /things in the night! No-one knows if they even actually make it home; sometimes people see/
--   /them sleeping outside the Blacksmith's home, others say they see them wandering towards the/
--   /woods. It's pointless quizzing the Village Drunk in the morning about their doings; they can/
--   /never remember what they did!/
--
--   The Village Drunk is initially aligned with the Villagers.
--
--   On the third night the Village Drunk sobers up and is randomly assigned a new alignment, either
--   Villagers or Werewolves.
villageDrunkRole :: Role
villageDrunkRole = Role
    { _name         = "Village Drunk"
    , _allegiance   = Villagers
    , _balance      = -1
    , _description  = T.unwords
        [ "Hah, maybe not as liked as the Jester, but the Drunk sure does their fair share of"
        , "stupid things in the night! No-one knows if they even actually make it home; sometimes"
        , "people see them sleeping outside the Blacksmith's home, others say they see them"
        , "wandering towards the woods. It's pointless quizzing the Village Drunk in the morning"
        , "about their doings; they can never remember what they did!"
        ]
    , _rules        = T.intercalate "\n"
        [ "The Village Drunk is initially aligned with the Villagers."
        , T.unwords
            [ "On the third night the Village Drunk sobers up and is randomly assigned a new"
            , "alignment, either Villagers or Werewolves."
            ]
        ]
    }

-- | /Long ago during the War in Heaven, angels fell from the sky as one by one those that followed/
--   /Lucifer were defeated. For centuries they lived amongst mortal Villagers as punishment for/
--   /their sins and wrongdoings. The Fallen Angel was one such being and is now one of the few/
--   /angels left on Earth. Nothing is worse punishment for them, the Fallen Angel yearns for death/
--   /to once again be free!/
--
--   When the Fallen Angel is in play, the game begins with the village's vote and then the first
--   night.
--
--   The Fallen Angel wins if they manage to get eliminated on the first round (day or night). If
--   however they fail, they become a Villager for the rest of the game.
fallenAngelRole :: Role
fallenAngelRole = Role
    { _name         = "Fallen Angel"
    , _allegiance   = NoOne
    , _balance      = 0
    , _description  = T.unwords
        [ "Long ago during the War in Heaven, angels fell from the sky as one by one those that"
        , "followed Lucifer were defeated. For centuries they lived amongst mortal Villagers as"
        , "punishment for their sins and wrongdoings. The Fallen Angel was one such being and is"
        , "now one of the few angels left on Earth. Nothing is worse punishment for them, the"
        , "Fallen Angel yearns for death to once again be free!"
        ]
    , _rules        = T.intercalate "\n"
        [ T.unwords
            [ "When the Fallen Angel is in play, the game begins with the village's vote and then"
            , "the first night."
            ]
        , T.unwords
            [ "The Fallen Angel wins if they manage to get eliminated on the first round (day or"
            , "night). If however they fail, they become a Villager for the rest of the game."
            ]
        ]
    }

-- | /Awareness comes easy to the Beholder. They listen to their senses and trust their hunches./
--   /Over the years the Beholder has grown to know a certain few of the village just by paying/
--   /attention. Little cues here and there, the way someone talks, the way they move - it all/
--   /gives clues as to their true nature and role./
--
--   At the start of the game the Beholder is informed the Seer's identity.
beholderRole :: Role
beholderRole = Role
    { _name         = "Beholder"
    , _allegiance   = Villagers
    , _balance      = 2
    , _description  = T.unwords
        [ "Awareness comes easy to the Beholder. They listen to their senses and trust their"
        , "hunches. Over the years the Beholder has grown to know a certain few of the village just"
        , "by paying attention. Little cues here and there, the way someone talks, the way they"
        , "move - it all gives clues as to their true nature and role."
        ]
    , _rules        = "At the start of the game the Beholder is informed the Seer's identity."
    }

-- | /Never trust a politician. Nor a Crooked Senator for that matter. The Crooked Senator may seem/
--   /like he has the village's best interests at heart, but let's be honest, when put in a tough/
--   /situation he looks after no-one but himself. Even when safe, the Crooked Senator may decide/
--   /to toy with the Villagers' emotions and try pit them against one another./
--
--   The Crooked Senator looks at the village votes as they come in.
crookedSenatorRole :: Role
crookedSenatorRole = Role
    { _name         = "Crooked Senator"
    , _allegiance   = Villagers
    , _balance      = 1
    , _description  = T.unwords
        [ "Never trust a politician. Nor a Crooked Senator for that matter. The Crooked Senator may"
        , "seem like he has the village's best interests at heart, but let's be honest, when put in"
        , "a tough situation he looks after no-one but himself. Even when safe, the Crooked Senator"
        , "may decide to toy with the Villagers' emotions and try pit them against one another."
        ]
    , _rules        = "The Crooked Senator looks at the village votes as they come in."
    }

-- | /How honoured we are to be in the presence of such a noble leader. The return of the Druid/
--   /marks an exceptional time in Fougères's history! Friend of the woodland creatures, practiced/
--   /philosopher and now, with the help of Ferina their companion, a bane to the Werewolves/
--   /themselves! My does she have a nose on her, strong enough to sniff out lycanthropes in close/
--   /proximity! Listen for her grunt and heed her warning for she will not let you down./
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
        , "lycanthropes in close proximity! Listen for her grunt and heed her warning for she will"
        , "not let you down."
        ]
    , _rules        = T.unwords
        [ "Each morning when Ferina wakes from her slumber she will be alert and cautious. If the"
        , "Druid is next to a Werewolf then Ferina will grunt in warning."
        ]
    }

-- | /A skilled marksman with quick reflexes. In the unfortunate situation that they are jumped and/
--   /killed unjustly, they let off a shot at their attacker, killing them instantly. The Hunter/
--   /never misses./
--
--   If the Hunter is killed they choose one player, believed to be an attacker, to kill
--   immediately.
hunterRole :: Role
hunterRole = Role
    { _name         = "Hunter"
    , _allegiance   = Villagers
    , _balance      = 2
    , _description  = T.unwords
        [ "A skilled marksman with quick reflexes. In the unfortunate situation that they are"
        , "jumped and killed unjustly, they let off a shot at their attacker, killing them"
        , "instantly. The Hunter never misses."
        ]
    , _rules        = T.unwords
        [ "If the Hunter is killed they choose one player, believed to be an attacker, to kill"
        , "immediately."
        ]
    }

-- | /Every village needs a Jester; they're so stupid, but provide so much entertainment! The/
--   /Jester may not have any special abilities, but at least no-one in the village wants to hurt/
--   /them./
--
--   If the village votes to lynch the Jester, their identity is revealed. The village realise
--   there's no point in burning them and so they are set free.
--
--   The Jester continues to play but may no longer vote as no-one can take them seriously.
jesterRole :: Role
jesterRole = Role
    { _name         = "Jester"
    , _allegiance   = Villagers
    , _balance      = 0
    , _description  = T.unwords
        [ "Every village needs a Jester; they're so stupid, but provide so much entertainment! The"
        , "Jester may not have any special abilities, but at least no-one in the village wants to"
        , "hurt them."
        ]
    , _rules        = T.intercalate "\n"
        [ T.unwords
            [ "If the village votes to lynch the Jester, their identity is revealed. The village"
            , "realise there's no point in burning them and so they are set free."
            ]
        , "The Jester continues to play but may no longer vote as no-one can take them seriously."
        ]
    }

-- | /Traditionally a Werewolf once transformed loses all memories and personality. Over years of/
--   /transforming, the Lycan has slowly evolved and learnt how to retain themself. Night after/
--   /night of devouring with the other Werewolves took its toll. The screams alone were enough to/
--   /turn the Lycan and make them question their true nature./
--
--   The Lycan is aligned with the Villagers, but appears to nature-seeing roles (e.g., the Seer) as
--   a Werewolf.
lycanRole :: Role
lycanRole = Role
    { _name         = "Lycan"
    , _allegiance   = Villagers
    , _balance      = 0
    , _description  = T.unwords
        [ "Traditionally a Werewolf once transformed loses all memories and personality. Over years"
        , "of transforming, the Lycan has slowly evolved and learnt how to retain themself. Night"
        , "after night of devouring with the other Werewolves took its toll. The screams alone were"
        , "enough to turn the Lycan and make them question their true nature."
        ]
    , _rules        = T.unwords
        [ "The Lycan is aligned with the Villagers, but appears to nature-seeing roles (e.g., the"
        , "Seer) as a Werewolf."
        ]
    }

-- | /The Protector is one of the few pure of heart and altruistic Villagers; they are forever/
--   /putting others needs above their own. Each night they fight against the Werewolves with/
--   /naught but a sword and shield, potentially saving an innocents life./
--
--   Each night the Protector chooses a player deemed worthy of their protection. That player is
--   safe for that night (and only that night) against the Werewolves.
--
--   The Protector may not protect the same player two nights in a row.
protectorRole :: Role
protectorRole = Role
    { _name         = "Protector"
    , _allegiance   = Villagers
    , _balance      = 2
    , _description  = T.unwords
        [ "The Protector is one of the few pure of heart and altruistic Villagers; they are forever"
        , "putting others needs above their own. Each night they fight against the Werewolves with"
        , "naught but a sword and shield, potentially saving an innocents life."
        ]
    , _rules        = T.intercalate "\n"
        [ T.unwords
            [ "Each night the Protector chooses a player deemed worthy of their protection. That"
            , "player is safe for that night (and only that night) against the Werewolves."
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
--   In this event, the Scapegoat has one last task to complete: they must choose whom is permitted
--   to vote or not on the next day.
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
            [ "In this event, the Scapegoat has one last task to complete: they must choose whom is"
            , "permitted to vote or not on the next day."
            ]
        ]
    }

-- | /The Seer has the ability to see into fellow townsfolk and determine their true nature. This/
--   /ability to see is not given out lightly, for certain it is a gift! The visions will always be/
--   /true, but only for the present as not even the Seer knows what the future holds./
--
--   Each night the Seer sees the allegiance of one player of their choice.
seerRole :: Role
seerRole = Role
    { _name         = "Seer"
    , _allegiance   = Villagers
    , _balance      = 2
    , _description  = T.unwords
        [ "The Seer has the ability to see into fellow townsfolk and determine their true nature."
        , "This ability to see is not given out lightly, for certain it is a gift! The visions will"
        , "always be true, but only for the present as not even the Seer knows what the future"
        , "holds."
        ]
    , _rules        = "Each night the Seer sees the allegiance of one player of their choice."
    }

-- | /A simple, ordinary townsperson in every way. Some may be cobblers, others bakers or even/
--   /nobles. No matter their differences though, the plight of Werewolves in Fougères unites all/
--   /the Villagers in this unfortunate time./
--
--   The Simple Villager has no special abilities, they must use their guile to determine whom among
--   them is not who they say they are.
simpleVillagerRole :: Role
simpleVillagerRole = Role
    { _name         = "Simple Villager"
    , _allegiance   = Villagers
    , _balance      = 1
    , _description  = T.unwords
        [ "A simple, ordinary townsperson in every way. Some may be cobblers, others bakers or even"
        , "nobles. No matter their differences though, the plight of Werewolves in Fougères unites"
        , "all the Villagers in this unfortunate time."
        ]
    , _rules        = T.unwords
        [ "The Simple Villager has no special abilities, they must use their guile to determine"
        , "whom among them is not who they say they are."
        ]
    }

-- | /The True Villager has a heart and soul as clear as day! Their allegiance and devotion to the/
--   /village are beyond reproach. If there is one person whom you should confide in, listen to and/
--   /trust, it is the True Villager./
--
--   At the start of the game the True Villager's identity is revealed.
trueVillagerRole :: Role
trueVillagerRole = Role
    { _name         = "True Villager"
    , _allegiance   = Villagers
    , _balance      = 2
    , _description  = T.unwords
        [ "The True Villager has a heart and soul as clear as day! Their allegiance and devotion to"
        , "the village are beyond reproach. If there is one person whom you should confide in,"
        , "listen to and trust, it is the True Villager."
        ]
    , _rules        = "At the start of the game the True Villager's identity is revealed."
    }

-- | /Somehow forgotten with the coming of the Werewolves, the Witch has a chance to prove themself/
--   /valuable to the village and maybe abolish the absurd pastime of burning and drowning their/
--   /cult. The Witch is blessed (or maybe cursed) with the ability to make two powerful potions;/
--   /one of which heals a victim of the Werewolves, the other poisons a player./
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
        , "themself valuable to the village and maybe abolish the absurd pastime of burning and"
        , "drowning their cult. The Witch is blessed (or maybe cursed) with the ability to make two"
        , "powerful potions; one of which heals a victim of the Werewolves, the other poisons a"
        , "player."
        ]
    , _rules        = T.unwords
        [ "The Witch is called after the Werewolves. They are able to heal and poison one player"
        , "per game. There is no restriction on using both potions in one night or to heal"
        , "themself."
        ]
    }

-- | /The Alpha Wolf leads the Werewolves in the raids against Fougères each night and not even the/
--   /Seer can see them coming. If the Werewolves caused the Villagers to question and accuse one/
--   /another beforehand, the Alpha Wolf eliminates any shred of humanity left. No-one can be/
--   /trusted anymore and no-one knows the truth./
--
--   The Alpha Wolf appears to nature-seeing roles (e.g., the Seer) as a Villager.
alphaWolfRole :: Role
alphaWolfRole = Role
    { _name         = "Alpha Wolf"
    , _allegiance   = Werewolves
    , _balance      = -5
    , _description  = T.unwords
        [ "The Alpha Wolf leads the Werewolves in the raids against Fougères each night and not"
        , "even the Seer can see them coming. If the Werewolves caused the Villagers to question"
        , "and accuse one another beforehand, the Alpha Wolf eliminates any shred of humanity left."
        , "No-one can be trusted anymore and no-one knows the truth."
        ]
    , _rules        =
        "The Alpha Wolf appears to nature-seeing roles (e.g., the Seer) as a Villager."
    }

-- | /The Simple Werewolf is a fearsome lupine, cunning like no other creature that roams the/
--   /forest. Their origin is unknown, but that matters little, for they present a grave threat to/
--   /Fougères. While each day they hide in plain sight as an ordinary Villager, each night they/
--   /transform and devour an innocent. There is little hope left for the village./
--
--   A Werewolf may never devour another Werewolf.
simpleWerewolfRole :: Role
simpleWerewolfRole = Role
    { _name         = "Simple Werewolf"
    , _allegiance   = Werewolves
    , _balance      = -4
    , _description  = T.unwords
        [ "The Simple Werewolf is a fearsome lupine, cunning like no other creature that roams the"
        , "forest. Their origin is unknown, but that matters little, for they present a grave"
        , "threat to Fougères. While each day they hide in plain sight as an ordinary Villager,"
        , "each night they transform and devour an innocent. There is little hope left for the"
        , "village."
        ]
    , _rules        = "A Werewolf may never devour another Werewolf."
    }
