{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

-- | Slightly more complicated than the 'Energy' example, we introduce
-- parallelism to the mix. The 'usingC' function shows us a "concurrent"
-- interface for combining witnesses, but we could just as easily produce a
-- secondary "sequential" combinator. We then have a vocabulary for expressing
-- the ordering of computation, and can freely mix together sequential and
-- parallel processes.
--
-- In this example, we imagine a networked game, where we must calculate the
-- winning team from 4 incoming requests that could arrive in any order. We
-- introduce a combinator to maximise the parallelism within this request
-- without touching the domain code. Of course, we could parameterise the
-- 'using' function with something with a name like @MonadDAG@, and fully
-- decouple evaluation from description.
module Test.Async where

import Control.Concurrent        (threadDelay)
import Control.Concurrent.Async  (concurrently)
import Dagmore                   as DM
import Data.Coerce               (coerce)
import Data.Functor.Identity     (Identity (..))
import System.Random             (randomIO)
import Test.QuickCheck.Arbitrary (Arbitrary)
import Type.Reflection           (Typeable)

-- | Player 1's score.
newtype Player1 = Player1 { get :: Int } deriving (Arbitrary, Eq, Show)

-- | Player 2's score.
newtype Player2 = Player2 { get :: Int } deriving (Arbitrary, Eq, Show)

-- | Player 3's score.
newtype Player3 = Player3 { get :: Int } deriving (Arbitrary, Eq, Show)

-- | Player 4's score.
newtype Player4 = Player4 { get :: Int } deriving (Arbitrary, Eq, Show)

-- | Team A is player 1 and 2.
newtype TeamAScore = TeamAScore { get :: Int } deriving (Eq, Show)

-- | Team A is player 3 and 4.
newtype TeamBScore = TeamBScore { get :: Int } deriving (Eq, Show)

-- | Who won the game?
data Result = TeamA | Draw | TeamB deriving (Eq, Show)

-- | The combined team A score is the sum of its players.
teamAScore :: Player1 -> Player2 -> TeamAScore
teamAScore = coerce @(Int -> _) (+)

-- | The combined team B score is, surprise surprise, the sum of its players.
teamBScore :: Player3 -> Player4 -> TeamBScore
teamBScore = coerce @(Int -> _) (+)

-- | The winning team is the one with the most points.
winner :: TeamAScore -> TeamBScore -> Result
winner this that
  = case coerce @(Int -> _) compare this that of
      LT -> TeamA
      EQ -> Draw
      GT -> TeamB

-- | In this example, we're tying together 'using' and 'combineWith', and these
-- need to have the same @h@ parameter throughout. This means we can't have it
-- universally-quantified: the @h@ in our DAG must be the same @h@ we had when
-- sequencing. See the 'usingC' function for an example, and try replacing the
-- explicit @h@ parameter with a @forall h.@ here to see what happens!
type DAG h a = DagmoreT h IO Identity (Witness h a)

-- | GLADIATORS
--
-- ARE
-- YOU
-- READY
main :: (Player1, Player2, Player3, Player4) -> IO Result
main (one, two, three, four) = runIdentity $ evaluate do
    player1 <- fakeNetwork one
    player2 <- fakeNetwork two
    player3 <- fakeNetwork three
    player4 <- fakeNetwork four

    teamA <- usingC (player1, player2) (uncurry teamAScore)
    teamB <- usingC (player3, player4) (uncurry teamBScore)

    result <- usingC (teamA, teamB) (uncurry winner)

    pure result

-- | Fake a network request just to prove that the ordering doesn't matter.
fakeNetwork :: Typeable a => a -> DAG h a
fakeNetwork value = DM.persist do
  delay <- randomIO
  threadDelay (delay `mod` 50)

  pure value

-- | Compute something using a bunch of witnesses whose computations are
-- concurrently executed.
usingC
  :: (Sequence h IO witnesses input, Typeable input, Typeable result)
  => witnesses -> (input -> result) -> DAG h result
usingC witnesses k = do
  aggregate <- DM.combineWith concurrently witnesses
  -- ^ For sequencential, use @DM.combine witnesses@

  DM.using aggregate (fmap k)
