{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | One extremely important structure in computation is the DAG, which is a:
--
-- * _Directed_. Relationships between "nodes" have a "direction". For example,
--   X /is the parent of/ Y, or X /is a type of/ Y.
-- 
-- * _Acyclic_. You can't follow a path and end up crossing the same "node"
--   more than once. For example, if X /owes money to/ Y, Y /owes money to/ Z,
--   and Z /owes money to/ X, there is a cycle.
--
-- * _Graph_. A series of "nodes" (in our case, these are just values of
--   distinct types) connected by some relationship (which, in our case, is
--   directed).
--
-- This module provides a way to represent DAGs using the much-more-familiar
-- @do@-notation, and exposes an API for the foundations of a DSL.
module Dagmore
  ( Witness
  , DagmoreT

  , Sequence (..)
  , Using    (..)

  , combine
  , evaluate
  , persist
  , using
  ) where

import qualified Dagmore.Sequence          as Sequence
import qualified Dagmore.Using             as Using
import           Data.Dynamic              (Dynamic)
import qualified Data.Dynamic              as Dynamic
import           Data.Kind                 (Type)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust)
import           Data.Proxy                (Proxy (..))
import           Data.Typeable             (Typeable)
import           Control.Applicative       (liftA2)
import           Control.Monad             ((<=<))
import           Control.Monad.Trans.State (StateT, evalStateT, get, modify)
import           Prelude                   hiding (lookup)
import           Type.Reflection           (SomeTypeRep, someTypeRep)

-- | As we collect results throughout our computation, the types of these
-- results will vary by necessity. In light of this, we have two options:
-- either we index our computation by the types computed within it, or we store
-- the values in a dynamic map such as this one - We can look for an @Int@
-- value by looking under the key of the type rep of @Int@!
--
-- Of course, as this is a 'Map' this has one distinct disadvantage over the
-- type-indexing method: every lookup comes with a 'Maybe'. However, we'll see
-- that a little rank-2 trick means we can guarantee successful lookups, and
-- hence use 'fromJust' without fear.
newtype Bag
  = Bag { debag :: Map SomeTypeRep Dynamic }
  deriving (Semigroup, Monoid, Show)

-- | We can insert any item into the 'Bag' as long as it has a known 'Typeable'
-- instance (allowing us to find the "fingerprint" for our type).
insert :: forall item. Typeable item => item -> Bag -> Bag
insert item
  = Bag . Map.insert (someTypeRep (Proxy @item)) (Dynamic.toDyn item) . debag

-- | We can look for values within the map by producing their fingerprint and
-- searching for that key. Note that this 'lookup' function has about half as
-- many arguments as the one we find in the @containers@ library: this is
-- because the /type of the result/ is really another argument! Hence, we'll
-- use @lookup bag :: Maybe Int@ or, more succinctly, @lookup @Int Bag@ to
-- specify "both" of our parameters.
lookup :: forall item. Typeable item => Bag -> Maybe item
lookup
  = Dynamic.fromDynamic <=< Map.lookup (someTypeRep (Proxy @item)) . debag

-- | We'll see that, within a computation, a 'Witness' should be treated as a
-- proof that a value exists within a bag, and the 'Maybe' is hence
-- unnecessary. The trick here is that a 'Witness' can't "escape" a computation
-- thanks to its @h@ parameter, and we don't expose this function to the
-- outside world. In other words, there's no way to call this function (even
-- indirectly) unless ithe value /definitely exists in the bag/.
find :: forall f h a. Typeable (f a) => Bag -> Witness h a -> f a
find bag _
  = fromJust (lookup {- @(f a) -} bag)

-- | A Dagmore computation exists within a bag-filled state transformer. This
-- means that, unbeknownst to our users, we're carrying a map full of values
-- persisted to the computation as we go along. We also index by the functor in
-- which the values' computations are performed: we might be using a
-- @RIO@-style monad to build up a computation in @Validation@, for example: we
-- don't necessarily need the stacks to match.
newtype DagmoreT (h :: Type) (f :: Type -> Type) (m :: Type -> Type) (a :: Type)
  = DagmoreT { runDagmoreT :: StateT Bag m a }
  deriving (Functor, Applicative, Monad)
  -- NB: MonadState/MonadReader should /not/ talk about 'Bag', and should be
  -- pass-through instances from 'm'. We don't want to expose the 'Bag' to our
  -- users!

-- | Witnesses are more "interesting" than proxies for two reasons:
--
-- * The constructor is hidden, so users can't produce witnesses directly.
--
-- * They're indexed by some type @h@, which ensures that they have a scope.
--   See the documentation for 'evaluate' to read more.
data Witness h a = Witness

-- | If we're going to be building computations, it's often helpful to compute
-- results using the results of /other/ computations. To this end, we can
-- compute one value /using/ the result of another computation.
--
-- As these witnesses will all have different types, we need a sort of
-- heterogeneous list to supply an input and output to whatever function we
-- write. This is fine, but for one problem: @HList@ syntax is
-- almost-unavoidably ugly. I say almost, because there is one ray of hope: we
-- can use tuples! This is good news – who doesn't like tuple syntax, after
-- all? – but, unlike Idris or PureScript, an n-tuple is not just sugar for
-- nested 2-tuples. This means we have to write a case of 'using' for /each
-- tuple/. Boo!
class Typeable f => Using h f input output | input f -> output where
  using' :: (forall x. Typeable x => Witness h x -> f x) -> input -> output

-- | For completeness, we'll supply an "empty tuple" case. We can think of
-- @persist x@ therefore as @using () \() -> x@.
instance Typeable f => Using h f () () where
  using' _ _ = ()

-- | When we /don't/ need multiple inputs to compute the next output, this case
-- lets us pass a @Witness@ directly, instead of a tuple.
instance (Typeable f, Typeable x)
    => Using h f (Witness h x) (f x) where
  using' f x = f x

-- I know what you're thinking: what about the other 63 possible tuples? Well,
-- we do have one last trick up our sleeves... Forgive me, padre:

$(Using.instances)

-- | If we have a value in our computation context, we can persist it to the
-- bag. On its own, this may not seem especially useful, given that we have
-- lexical scope in Haskell. However, this is probably more suitable as a
-- function within a DSL layered on top of the DAGs.
persist :: (Monad m, Typeable (f a)) => f a -> DagmoreT h f m (Witness h a)
persist = fmap (const Witness) . DagmoreT . modify . insert

-- | The next API that we expose to our users for dependent computations is
-- much less frightening than the above, despite the noisy type signature. "If
-- you give me some number of witnesses, and a function that computes something
-- based on those inputs, I'll give you a witness to the output". Of course,
-- how the output is computed is entirely up to the user: we can do something
-- concurrently, or skip a computation, or anything else we might like!
using
  :: forall h m f witness input output
   . ( Monad m
     , Typeable output
     , Using h f witness input
     )
  => witness
  -> (input -> f output)
  -> DagmoreT h f m (Witness h output)

using witnesses k = DagmoreT get >>= \bag ->
  persist (k (using' (find @f @h bag) witnesses))

-- | We can extract the result of a Dagmore computation within its context by
-- looking up the value under the witness.
--
-- The interesting part of this function is the @forall h.@ around the input:
-- here, we're enforcing that the @h@ variable can't "escape" the computation.
-- Imagine, for example, that it wasn't there:
--
-- * We persist a witness to our store. @persist (pure (Witness @anything))@
--
-- * We 'evaluate' our witness-resulting computation, @evaluate theAbove@, and
--   end up with @m (f (Witness anything))@.
--
-- * We start a new computation when our witness is in scope.
--   @theAbove >>= evaluate do ...
--
-- * In this computation, we use our first witness. @using myWitness \_ -> ...@
--
-- * RUNTIME ERROR! The witness points to a value that isn't there!
--
-- It's clear that witnesses only make sense within the scope of the
-- computation that creates them. If they can escape, we can use them in
-- computations where the witnessed value /hasn't/ been computed, and we'll get
-- crashes.
--
-- So, why does the @h@ help? Well, let's look at that second step again. If we
-- evaluate our computation, we have:
--
-- @
--   evaluate
--     :: (forall h. DagmoreT h f m (Witness h (Witness h output)))
--     -> m (f (Witness h output))
-- @
-- 
-- Uh oh! This doesn't make sense - our @h@ is /scoped/ to the input value, and
-- can't be referenced in the output! We can't simply give it a new name,
-- either, because then the types won't match. By having this extra index, we
-- ensure that a @Witness@ can never leave the computation in which it was
-- created, which means we can /know/ they'll always be valid.
evaluate
  :: forall f m output
   . ( Monad m
     , Typeable f
     , Typeable output
     )
  => (forall h. DagmoreT h f m (Witness h output))
  -> m (f output)

evaluate dagmore
  = flip evalStateT mempty do
      witness <- runDagmoreT dagmore
      bag     <- get

      pure (find bag witness)

-- | 'evaluate' is a nice interface, but it limits us to single witnesses as a
-- return value. This isn't a problem, though, as we can sneak round the
-- restriction:
--
-- @
--   using (this, that) \(x, y) -> liftA2 (,) x y
-- @
--
-- Here, we've used the applicative instance of our @f@ context to combine the
-- two witnesses using @liftA2@. We can factor this out as a user-supplied
-- function to remove the @Applicative@ constraint (e.g. I may be using 'IO'
-- and want to combine these using something like @concurrently@).
--
-- The class generalises this form to n-ary tuples, just as 'using' does. This
-- hopefully makes it much neater to combine witnesses.
class Sequence f h input output | input -> output where

  -- | Take a tuple of witnesses, and turn it into a witness of a tuple, using
  -- some user-supplied "zipping" function.
  combineWith
    :: ( Typeable f
       , Monad m
       )
    => (forall x y. f x -> f y -> f (x, y))
    -> input
    -> DagmoreT h f m (Witness h output)

instance Applicative f => Sequence f h () () where
  combineWith _ = flip using pure

instance Typeable one => Sequence f h (Witness h one) one where
  combineWith _ = flip using id

$(Sequence.instances)

-- | Of course, we often /don't/ need anything cleverer than the 'Applicative'
-- instance, so this function provides this common case.
combine
  :: ( Applicative f
     , Typeable f
     , Sequence f h input output
     , Monad m
     )
  => input
  -> DagmoreT h f m (Witness h output)

combine
  = combineWith (liftA2 (,))
