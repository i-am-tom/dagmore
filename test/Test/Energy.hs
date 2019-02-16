{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

-- | Here's a very simple example of a DAG computation. The important things to
-- notice here are:
--
-- * We're in an 'Identity' context, so there's nothing really "going on"
--   behind the scenes. We don't really need a DAG for this one at all, but
--   it's a nice introductory example.
--
-- * The different units all have different /types/ thanks to the 'Some'
--   index, which means they don't replace each other in the DAG.
--
-- * We use the 'Applicative' interface to combine different witnesses.
module Test.Energy where

import Dagmore               (DagmoreT, Witness, evaluate, persist, using)
import Data.Coerce           (coerce)
import Data.Functor.Identity (Identity (..))
import Test.QuickCheck       (Arbitrary)
import Type.Reflection       (Typeable)

-- | The types of quantities we'll be using today.
data Quantity = Acceleration | Displacement | Energy | Force | Mass

-- | A specialised version of the @Tagged (x :: k) a@ type that the @tagged@
-- library exposes.
newtype Some (which :: Quantity)
  = Some { runSome :: Double }
  deriving (Arbitrary, Eq, Show)

-- | Basically 'coerce (*)', but with specialised types.
mkForce :: Some 'Acceleration -> Some 'Mass -> Some 'Force
mkForce = coerce @(Double -> _) (*)

-- | ... Also basically 'coerce (*)', but with specialised types.
mkEnergy :: Some 'Force -> Some 'Displacement -> Some 'Energy
mkEnergy = coerce @(Double -> _) (*)

-- | In our case, we have just about the least interesting possible DAG. Both
-- the construction and evaluation contexts are 'Identity', and the @h@ is
-- universally-quantified, so we only actually have one parameter of "freedom".
type DAG a = forall h. DagmoreT h Identity Identity (Witness h a)

-- | Take some default (quick-checkable) values and do a basic calculation!
main :: (Some 'Acceleration, Some 'Mass, Some 'Displacement) -> Some 'Energy
main (a, m, d) = run do
    acceleration <- register a
    mass         <- register m
    displacement <- register d

    force  <- using (acceleration, mass)  \(a', m') -> mkForce  <$> a' <*> m'
    energy <- using (force, displacement) \(f', d') -> mkEnergy <$> f' <*> d'

    pure energy

  where
    run :: DAG (Some 'Energy) -> Some 'Energy
    run x = runIdentity . runIdentity $ evaluate x

    register :: Typeable a => a -> DAG a
    register = persist . Identity
