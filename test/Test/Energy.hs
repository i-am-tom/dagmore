{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

-- | Here's
module Test.Energy where

import Dagmore               (DagmoreT, Witness, evaluate, persist, using)
import Data.Coerce           (coerce)
import Data.Functor.Identity (Identity (..))
import Test.QuickCheck       (Arbitrary)
import Control.Monad         (join)
import Type.Reflection       (Typeable)

data Quantity
  = Acceleration
  | Displacement
  | Energy
  | Force
  | Mass

newtype Tagged (which :: Quantity)
  = Tagged { runTagged :: Double }
  deriving (Arbitrary, Eq, Show)

mkForce :: Tagged 'Acceleration -> Tagged 'Mass -> Tagged 'Force
mkForce = coerce @(Double -> _) (*)

mkEnergy :: Tagged 'Force -> Tagged 'Displacement -> Tagged 'Energy
mkEnergy = coerce @(Double -> _) (*)

type DAG a = forall h. DagmoreT h Identity Identity (Witness h a)

main
  :: ( Tagged 'Acceleration
     , Tagged 'Mass
     , Tagged 'Displacement
     )
  -> Tagged 'Energy

main (a, m, d) = run do
    acceleration <- register a
    mass         <- register m
    displacement <- register d

    force <- using (acceleration, mass) \(a', m') ->
      mkForce <$> a' <*> m'

    using (force, displacement) \(f', d') ->
      mkEnergy <$> f' <*> d'
  where
    run :: DAG (Tagged 'Energy) -> Tagged 'Energy
    run x = runIdentity . runIdentity $ evaluate x

    register :: Typeable a => a -> DAG a
    register = persist . Identity
