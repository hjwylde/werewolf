{-|
Module      : Game.Werewolf.Role
Description : Simplistic role data structure with lenses and instances.

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
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Werewolf.Role (
    -- * Role
    Role,
    tag, name, allegiance, balance, activity, description, rules,

    Allegiance(..),
    _NoOne, _Villagers, _Werewolves,

    Activity(..),
    _Diurnal, _Nocturnal,

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
    dullahanRole, fallenAngelRole, necromancerRole, zombieRole,

    -- *** The Villagers
    -- | Fraught with fear of the unseen enemy, the Villagers must work together to determine the
    --   truth and eliminate the threat to Fougères. The task before them will not be easy, but a
    --   certain few have learnt some tricks over the years that may turn out rather useful.

    --   The Villagers must lynch all of the Werewolves.
    beholderRole, crookedSenatorRole, druidRole, hunterRole, jesterRole, lycanRole, medusaRole,
    oracleRole, protectorRole, saintRole, scapegoatRole, seerRole, simpleVillagerRole,
    spitefulVillagerRole, trueVillagerRole, witchRole,

    -- *** The Werewolves
    -- | Hiding in plain sight, the Werewolves are not a small trifle.

    --   The Werewolves must devour all of the Villagers.
    alphaWolfRole, simpleWerewolfRole,
) where

import Control.Lens

import Data.Function
import Data.List
import Data.String.Humanise
import Data.String.Interpolate.Extra
import Data.Text                     as T

-- | Role definitions require only a few pieces of information.
--
--   The @balance@ attribute on a role indicates the allegiance it favours. For example, a Simple
--   Werewolf has a balance of -4 while the Seer has a balance of 2. A balance of 0 means it favours
--   neither allegiance.
--
--   N.B., role equality is defined on just the 'tag' as a role's 'allegiance' may change throughout
--   the game.
data Role = Role
    { _tag         :: Text
    , _name        :: Text
    , _allegiance  :: Allegiance
    , _balance     :: Int
    , _activity    :: Activity
    , _description :: Text
    , _rules       :: Text
    } deriving (Read, Show)

-- | The 'NoOne' allegiance is used for the Loners. It is not used to determine who has won (i.e.,
--   if one Loner wins, the others still lose).
data Allegiance = NoOne | Necromancer | Villagers | Werewolves
    deriving (Eq, Read, Show)

instance Humanise Allegiance where
    humanise NoOne          = "no-one"
    humanise Necromancer    = "Necromancer"
    humanise Villagers      = "Villagers"
    humanise Werewolves     = "Werewolves"

-- | Defines whether a role is diurnal or nocturnal. I.e., if the role's turn occurs during the day
--   or night.
data Activity = Diurnal | Nocturnal
    deriving (Eq, Read, Show)

instance Humanise Activity where
    humanise Diurnal    = "diurnal"
    humanise Nocturnal  = "nocturnal"

makePrisms ''Allegiance

makePrisms ''Activity

makeLenses ''Role

instance Eq Role where
    (==) = (==) `on` view tag

instance Humanise Role where
    humanise = view name

-- | A list containing all the roles defined in this file.
allRoles :: [Role]
allRoles =
    [ alphaWolfRole
    , beholderRole
    , crookedSenatorRole
    , druidRole
    , dullahanRole
    , fallenAngelRole
    , hunterRole
    , jesterRole
    , lycanRole
    , medusaRole
    , necromancerRole
    , oracleRole
    , orphanRole
    , protectorRole
    , saintRole
    , scapegoatRole
    , seerRole
    , simpleVillagerRole
    , simpleWerewolfRole
    , spitefulVillagerRole
    , trueVillagerRole
    , villageDrunkRole
    , witchRole
    , zombieRole
    ]

-- | A list containing roles that are restricted to a single instance per 'Game'.
--
--   @
--   'restrictedRoles' = 'allRoles' \\\\ ['simpleVillagerRole', 'simpleWerewolfRole', 'spitefulVillagerRole', 'zombieRole']
--   @
restrictedRoles :: [Role]
restrictedRoles = allRoles \\ [simpleVillagerRole, simpleWerewolfRole, spitefulVillagerRole, zombieRole]

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
    { _tag          = "orphan"
    , _name         = T.strip [iFile|variant/standard/role/orphan/name.txt|]
    , _allegiance   = Villagers
    , _balance      = -3
    , _activity     = Nocturnal
    , _description  = T.strip [iFile|variant/standard/role/orphan/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/orphan/rules.txt|]
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
    { _tag          = "village-drunk"
    , _name         = T.strip [iFile|variant/standard/role/village-drunk/name.txt|]
    , _allegiance   = Villagers
    , _balance      = -3
    , _activity     = Diurnal
    , _description  = T.strip [iFile|variant/standard/role/village-drunk/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/village-drunk/rules.txt|]
    }

-- | /Normally the Dullahan carries their head under one arm, however while amongst the Villagers,/
--   /they ere on the side of caution and rest it in a more traditional place. The Dullahan rides a/
--   /black horse as dark as night and hunts down travellers in the countryside. Beware if the/
--   /Dullahan knows your name, for you are then marked for death and you should avoid them at all/
--   /costs.
--
--   The Dullahan is given a list of player names at the start of the game. To win, they must
--   eliminate all of them before the end of the game.
dullahanRole :: Role
dullahanRole = Role
    { _tag          = "dullahan"
    , _name         = T.strip [iFile|variant/standard/role/dullahan/name.txt|]
    , _allegiance   = NoOne
    , _balance      = 0
    , _activity     = Diurnal
    , _description  = T.strip [iFile|variant/standard/role/dullahan/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/dullahan/rules.txt|]
    }

-- | /Long ago during the War in Heaven, angels fell from the sky as one by one those that followed/
--   /Lucifer were defeated. For centuries they lived amongst mortal Villagers as punishment for/
--   /their sins and wrongdoings. The Fallen Angel was one such being and is now one of the few/
--   /angels left on Earth. Nothing is worse punishment for them, the Fallen Angel yearns for death/
--   /to once again be free!/
--
--   The Fallen Angel wins if they manage to get lynched by the Villagers before the end of the
--   game.
fallenAngelRole :: Role
fallenAngelRole = Role
    { _tag          = "fallen-angel"
    , _name         = T.strip [iFile|variant/standard/role/fallen-angel/name.txt|]
    , _allegiance   = NoOne
    , _balance      = 0
    , _activity     = Diurnal
    , _description  = T.strip [iFile|variant/standard/role/fallen-angel/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/fallen-angel/rules.txt|]
    }

-- | /The dead are feared among the living; the dead outnumber the living. In most villages the/
--   /dead remain that way, but not when the Necromancer is present. The Necromancer devoted their/
--   /life to learning black magic and methods of bringing people back to life. Unfortunately this/
--   /art is hard to perfect and all they can manage to bring back are soulless Zombies./
--
--   Once per game the Necromancer can choose to resurrect all dead players as Zombies. The Zombies
--   are aligned with the Necromancer and they cannot be lynched or devoured.
--
--   If the Necromancer is killed, all Zombies die with them.
--
--   The Necromancer and Zombies win if they are the last ones alive.
necromancerRole :: Role
necromancerRole = Role
    { _tag          = "necromancer"
    , _name         = T.strip [iFile|variant/standard/role/necromancer/name.txt|]
    , _allegiance   = NoOne
    , _balance      = 0
    , _activity     = Nocturnal
    , _description  = T.strip [iFile|variant/standard/role/necromancer/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/necromancer/rules.txt|]
    }

-- | /A loyal follower of the Necromancer. A Zombie has no mind of its own and blindly obeys every/
--   /command of their master./
--
--   A Zombie wins with the Necromancer. They cannot be killed, however they die when the
--   Necromancer dies.
zombieRole :: Role
zombieRole = Role
    { _tag          = "zombie"
    , _name         = T.strip [iFile|variant/standard/role/zombie/name.txt|]
    , _allegiance   = Necromancer
    , _balance      = 0
    , _activity     = Nocturnal
    , _description  = T.strip [iFile|variant/standard/role/zombie/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/zombie/rules.txt|]
    }

-- | /Awareness comes easy to the Beholder. They listen to their senses and trust their hunches./
--   /Over the years the Beholder has grown to know a certain few of the village just by paying/
--   /attention. Little cues here and there, the way someone talks, the way they move - it all/
--   /gives clues as to their true nature and role./
--
--   At the start of the game the Beholder is informed the Seer's identity.
beholderRole :: Role
beholderRole = Role
    { _tag          = "beholder"
    , _name         = T.strip [iFile|variant/standard/role/beholder/name.txt|]
    , _allegiance   = Villagers
    , _balance      = 2
    , _activity     = Diurnal
    , _description  = T.strip [iFile|variant/standard/role/beholder/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/beholder/rules.txt|]
    }

-- | /Never trust a politician. Nor a Crooked Senator for that matter. The Crooked Senator may seem/
--   /like he has the village's best interests at heart, but let's be honest, when put in a tough/
--   /situation he looks after no-one but himself. Even when safe, the Crooked Senator may decide/
--   /to toy with the Villagers' emotions and try pit them against one another./
--
--   The Crooked Senator looks at the village votes as they come in.
crookedSenatorRole :: Role
crookedSenatorRole = Role
    { _tag          = "crooked-senator"
    , _name         = T.strip [iFile|variant/standard/role/crooked-senator/name.txt|]
    , _allegiance   = Villagers
    , _balance      = 2
    , _activity     = Diurnal
    , _description  = T.strip [iFile|variant/standard/role/crooked-senator/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/crooked-senator/rules.txt|]
    }

-- | /How honoured we are to be in the presence of such a noble leader. The return of the Druid/
--   /marks an exceptional time in Fougères's history! Friend of the woodland creatures, practiced/
--   /philosopher and now, with the help of Ferina their companion, a bane to the Werewolves/
--   /themselves! My does she have a nose on her, strong enough to sniff out lycanthropes in close/
--   /proximity! Listen for her grunt and heed her warning for she will not let you down./
--
--   Each morning when Ferina wakes from her slumber she will be alert and cautious. If the Druid is
--   next to a Werewolf in the player `circle` then Ferina will grunt in warning.
druidRole :: Role
druidRole = Role
    { _tag          = "druid"
    , _name         = T.strip [iFile|variant/standard/role/druid/name.txt|]
    , _allegiance   = Villagers
    , _balance      = 5
    , _activity     = Diurnal
    , _description  = T.strip [iFile|variant/standard/role/druid/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/druid/rules.txt|]
    }

-- | /A skilled marksman with quick reflexes. In the unfortunate situation that they are jumped and/
--   /killed unjustly, they let off a shot at their attacker, killing them instantly. The Hunter/
--   /never misses./
--
--   If the Hunter is killed they choose one player, believed to be an attacker, to kill
--   immediately.
hunterRole :: Role
hunterRole = Role
    { _tag          = "hunter"
    , _name         = T.strip [iFile|variant/standard/role/hunter/name.txt|]
    , _allegiance   = Villagers
    , _balance      = 3
    , _activity     = Diurnal
    , _description  = T.strip [iFile|variant/standard/role/hunter/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/hunter/rules.txt|]
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
    { _tag          = "jester"
    , _name         = T.strip [iFile|variant/standard/role/jester/name.txt|]
    , _allegiance   = Villagers
    , _balance      = 1
    , _activity     = Diurnal
    , _description  = T.strip [iFile|variant/standard/role/jester/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/jester/rules.txt|]
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
    { _tag          = "lycan"
    , _name         = T.strip [iFile|variant/standard/role/lycan/name.txt|]
    , _allegiance   = Villagers
    , _balance      = -1
    , _activity     = Diurnal
    , _description  = T.strip [iFile|variant/standard/role/lycan/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/lycan/rules.txt|]
    }

-- | /A beautiful flirt, the Medusa is aligned with the Villagers but harbours a terrifying secret./
--   /During the day they are well known in the village of Fougères for their stunning appearance/
--   /which captures the eye and love of all the townsfolk. However when their secret takes ahold/
--   /at sundown, their true self is revealed. Any who gaze upon her true form would see live/
--   /snakes for hair and the few that further look into her eyes are turned to stone./
--
--   If Medusa attracts the attention of a Werewolf during the night and is devoured, the first
--   Werewolf to their left in the player `circle` will catch their gaze and turn to stone,
--   instantly killing the lupine predator.
medusaRole :: Role
medusaRole = Role
    { _tag          = "medusa"
    , _name         = T.strip [iFile|variant/standard/role/medusa/name.txt|]
    , _allegiance   = Villagers
    , _balance      = 4
    , _activity     = Diurnal
    , _description  = T.strip [iFile|variant/standard/role/medusa/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/medusa/rules.txt|]
    }

-- | /Originally rejected by the townsfolk, the Oracle's prophetic divinations has earned trust/
--   /within the village. With constant precognition - and concern for the future - the Oracle/
--   /knows the village will only live if they work together./
--
--   Each night the Oracle chooses a player to divine. They are then informed of the player's role
--   the following morning. This wisdom is for the Oracle to use to ensure the future of Fougères.
oracleRole :: Role
oracleRole = Role
    { _tag          = "oracle"
    , _name         = T.strip [iFile|variant/standard/role/oracle/name.txt|]
    , _allegiance   = Villagers
    , _balance      = 4
    , _activity     = Nocturnal
    , _description  = T.strip [iFile|variant/standard/role/oracle/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/oracle/rules.txt|]
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
    { _tag          = "protector"
    , _name         = T.strip [iFile|variant/standard/role/protector/name.txt|]
    , _allegiance   = Villagers
    , _balance      = 3
    , _activity     = Nocturnal
    , _description  = T.strip [iFile|variant/standard/role/protector/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/protector/rules.txt|]
    }

-- | /The Saint, also historically known as a hallow, is recognized as having an exceptional degree of/
--   /holiness and likeness to God. They are a humble Villager and shine light on these dark times./
--   /Extinguishing this light would not be wise/
--
--   If the Saint is lynched by the village, all who voted for them die.
saintRole :: Role
saintRole = Role
    { _tag          = "saint"
    , _name         = T.strip [iFile|variant/standard/role/saint/name.txt|]
    , _allegiance   = Villagers
    , _balance      = 3
    , _activity     = Diurnal
    , _description  = T.strip [iFile|variant/standard/role/saint/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/saint/rules.txt|]
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
    { _tag          = "scapegoat"
    , _name         = T.strip [iFile|variant/standard/role/scapegoat/name.txt|]
    , _allegiance   = Villagers
    , _balance      = 0
    , _activity     = Diurnal
    , _description  = T.strip [iFile|variant/standard/role/scapegoat/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/scapegoat/rules.txt|]
    }

-- | /The Seer has the ability to see into fellow townsfolk and determine their true nature. This/
--   /ability to see is not given out lightly, for certain it is a gift! The visions will always be/
--   /true, but only for the present as not even the Seer knows what the future holds./
--
--   Each night the Seer sees the allegiance of one player of their choice.
seerRole :: Role
seerRole = Role
    { _tag          = "seer"
    , _name         = T.strip [iFile|variant/standard/role/seer/name.txt|]
    , _allegiance   = Villagers
    , _balance      = 4
    , _activity     = Nocturnal
    , _description  = T.strip [iFile|variant/standard/role/seer/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/seer/rules.txt|]
    }

-- | /A simple, ordinary townsperson in every way. Some may be cobblers, others bakers or even/
--   /nobles. No matter their differences though, the plight of Werewolves in Fougères unites all/
--   /the Villagers in this unfortunate time./
--
--   The Simple Villager has no special abilities, they must use their guile to determine whom among
--   them is not who they say they are.
simpleVillagerRole :: Role
simpleVillagerRole = Role
    { _tag          = "simple-villager"
    , _name         = T.strip [iFile|variant/standard/role/simple-villager/name.txt|]
    , _allegiance   = Villagers
    , _balance      = 1
    , _activity     = Diurnal
    , _description  = T.strip [iFile|variant/standard/role/simple-villager/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/simple-villager/rules.txt|]
    }

-- | /A simple, ordinary townsperson in every way. Some may be cobblers, others bakers or even/
--   /nobles. No matter their differences though, the plight of Werewolves in Fougères unites all/
--   /the Villagers in this unfortunate time./
--
--   /Yet the Spiteful Villager has no loyalty in the afterlife; whoever causes them harm may find/
--   /themselves in trouble./
--
--   When the Spiteful Villager is killed, they are informed of everyone's roles and may haunt the
--   village as they wish.
spitefulVillagerRole :: Role
spitefulVillagerRole = Role
    { _tag          = "spiteful-villager"
    , _name         = T.strip [iFile|variant/standard/role/spiteful-villager/name.txt|]
    , _allegiance   = Villagers
    , _balance      = 1
    , _activity     = Diurnal
    , _description  = T.strip [iFile|variant/standard/role/spiteful-villager/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/spiteful-villager/rules.txt|]
    }

-- | /The True Villager has a heart and soul as clear as day! Their allegiance and devotion to the/
--   /village are beyond reproach. If there is one person whom you should confide in, listen to and/
--   /trust, it is the True Villager./
--
--   At the start of the game the True Villager's identity is revealed.
trueVillagerRole :: Role
trueVillagerRole = Role
    { _tag          = "true-villager"
    , _name         = T.strip [iFile|variant/standard/role/true-villager/name.txt|]
    , _allegiance   = Villagers
    , _balance      = 2
    , _activity     = Diurnal
    , _description  = T.strip [iFile|variant/standard/role/true-villager/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/true-villager/rules.txt|]
    }

-- | /Somehow forgotten with the coming of the Werewolves, the Witch has a chance to prove themself/
--   /valuable to the village and maybe abolish the absurd pastime of burning and drowning their/
--   /cult. The Witch is blessed (or maybe cursed) with the ability to make two powerful potions;/
--   /one of which heals a victim of the Werewolves, the other poisons a player./
--
--   The Witch is called after the Werewolves. They are able to heal and poison one player per game.
--   There is no restriction on using both potions in one night or on healing themself.
witchRole :: Role
witchRole = Role
    { _tag          = "witch"
    , _name         = T.strip [iFile|variant/standard/role/witch/name.txt|]
    , _allegiance   = Villagers
    , _balance      = 3
    , _activity     = Nocturnal
    , _description  = T.strip [iFile|variant/standard/role/witch/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/witch/rules.txt|]
    }

-- | /The Alpha Wolf leads the Werewolves in the raids against Fougères each night and not even the/
--   /Seer can see them coming. If the Werewolves caused the Villagers to question and accuse one/
--   /another beforehand, the Alpha Wolf eliminates any shred of humanity left. No-one can be/
--   /trusted anymore and no-one knows the truth./
--
--   The Alpha Wolf appears to nature-seeing roles (e.g., the Seer) as a Villager.
alphaWolfRole :: Role
alphaWolfRole = Role
    { _tag          = "alpha-wolf"
    , _name         = T.strip [iFile|variant/standard/role/alpha-wolf/name.txt|]
    , _allegiance   = Werewolves
    , _balance      = -7
    , _activity     = Nocturnal
    , _description  = T.strip [iFile|variant/standard/role/alpha-wolf/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/alpha-wolf/rules.txt|]
    }

-- | /The Simple Werewolf is a fearsome lupine, cunning like no other creature that roams the/
--   /forest. Their origin is unknown, but that matters little, for they present a grave threat to/
--   /Fougères. While each day they hide in plain sight as an ordinary Villager, each night they/
--   /transform and devour an innocent. There is little hope left for the village./
--
--   A Werewolf may never devour another Werewolf.
simpleWerewolfRole :: Role
simpleWerewolfRole = Role
    { _tag          = "simple-werewolf"
    , _name         = T.strip [iFile|variant/standard/role/simple-werewolf/name.txt|]
    , _allegiance   = Werewolves
    , _balance      = -5
    , _activity     = Nocturnal
    , _description  = T.strip [iFile|variant/standard/role/simple-werewolf/description.txt|]
    , _rules        = T.strip [iFile|variant/standard/role/simple-werewolf/rules.txt|]
    }
