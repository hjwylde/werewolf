{-|
Module      : Game.Werewolf.Test.Command
Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Game.Werewolf.Test.Command (
    -- * Tests
    allCommandTests,
) where

import Game.Werewolf.Test.Command.Choose
import Game.Werewolf.Test.Command.Heal
import Game.Werewolf.Test.Command.Pass
import Game.Werewolf.Test.Command.Poison
import Game.Werewolf.Test.Command.Protect
import Game.Werewolf.Test.Command.Quit
import Game.Werewolf.Test.Command.Reveal
import Game.Werewolf.Test.Command.See
import Game.Werewolf.Test.Command.Vote

import Test.Tasty

allCommandTests :: [TestTree]
allCommandTests = concat
    [ allChooseCommandTests
    , allHealCommandTests
    , allPassCommandTests
    , allPoisonCommandTests
    , allProtectCommandTests
    , allQuitCommandTests
    , allRevealCommandTests
    , allSeeCommandTests
    , allVoteCommandTests
    ]
