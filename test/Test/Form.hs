{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE KindSignatures             #-}

-- | In /this/ example, we examine a common problem in validation: we want to
-- collect as many errors as possible when validating (which suggests an
-- 'Applicative'-style validation), but we want to validate /some/ things based
-- on others (which suggests an 'Either'-style validation).
--
-- There are several approaches to this problem, not all of them lawful: types
-- like those found in Haxl opt for a slightly-unlawful 'Applicative' instance,
-- or we might introduce functions like 'andThen' below to provide a
-- "bind-like" operation. In /this/ example, we assume that any items being
-- passed to the 'with' function can be validated applicatively, but the action
-- happening upon those items "binds" with `andThen`. This means that we have a
-- nice, declarative interface for validating input.
module Test.Form where

import Dagmore
import Data.Either.Validation (Validation (..))
import Data.Functor.Identity  (Identity (..))
import Data.Kind              (Type)
import Data.Maybe             (isNothing)
import Text.Read              (readMaybe)
import Type.Reflection        (Typeable)

-- | Let's start with the example, and cover the machinery afterwards. Here,
-- for the sake of the tests, we take three string inputs (though, as with the
-- others, this is /just for the tests/: how those inputs are gathered is not
-- our concern.
--
-- We then validate each of them as separate pieces of information. Before
-- returning a validated object, however, we have a condition: every user over
-- the age of 16 must enter a surname. To do this, we requice that everything
-- so far was validated successfully. If this be the case, we perform that age
-- check. If not, we collect all the errors from the component parts.
main :: (String, String, String) -> Validation [String] User
main (fn, sn, a) = runIdentity $ evaluate do
  forename <- validate fn
  surname  <- validate sn
  age      <- validate a

  -- Applicatively validate the three, and then "bind" them into a further
  -- result.
  with (forename, age, surname) \(forename', age', surname') ->
    if age' > 16 && isNothing surname'
      then Failure   [ "No surname!" ]
      else Success $ User forename' surname' age'

-- | In order to do the above, we have to introduce a well-known combinator for
-- 'Applicative' types of this style: @andThen@ returns the first argument if
-- it's a failure, or the result of running the function on its success.
andThen :: Validation e a -> (a -> Validation e b) -> Validation e b
andThen x f = case x of
  Failure y -> Failure y
  Success y -> f y

-- | For the purposes of this demo, let's introduce some validation class. Of
-- course, in a real application, this will likely be much more complicated.
class Validateable (clean :: Type) where
  validate' :: String -> Validation [String] clean

-- | We introduce 'Maybe' as the indicator of /allowable failure/. Again, an
-- actual application will most likely have different rules akin to, "validate
-- if present", rather than this.
instance Validateable a => Validateable (Maybe a) where
  validate' x
    = case validate' x of
        Success y -> Success $ Just y
        Failure _ -> Success   Nothing

-- | The type of a first name.
newtype Forename = Forename { get :: String }
  deriving (Eq, Ord, Show)

-- | The only thing we require of a first name for this example is that it be
-- non-empty.
instance Validateable Forename where
  validate' = \case [] -> Failure   [ "Empty forename" ]
                    xs -> Success $ Forename xs

-- | Surnames are basically the same as first names, and we'll borrow the first
-- name validation while we're here.
newtype Surname = Surname { get :: String }
  deriving (Eq, Ord, Show)
  deriving Validateable via Forename

-- | The age of the user.
newtype Age = Age { get :: Int }
  deriving (Eq, Ord, Num, Show)

-- | We read the input as an int, and then check it's positive.
instance Validateable Age where
  validate' x
    = case readMaybe x of
        Just y -> if y > 0
          then Success $ Age y
          else Failure   [ "Negative age!" ]

        Nothing ->
          Failure [ "Couldn't parse age!" ]

-- | Finally, users are just a collection of a first name, an age, and possibly
-- a surname.
data User
  = User
      { _forename :: Forename
      , _surname  :: Maybe Surname
      , _age      :: Age
      }
  deriving (Eq, Show)

-- | A convenient synonym, as we saw in the 'Async' example.
type DAG h a = DagmoreT h (Validation [String]) Identity (Witness h a)

-- | We can "validate" inputs to produce witnesses with the type's
-- 'Validateable' instance.
validate :: (Typeable a, Validateable a) => String -> DAG h a
validate = persist . validate'

-- | This function looks a lot like 'Test.Async.usingC' except that it uses the
-- 'Applicative' instance of 'Validation' to validate, but the function
-- thereafter looks more like a '>>=' than an '<$>'. This means we can perform
-- validation that is determined by other inputs.
with
  :: (Sequence h (Validation [String]) witnesses input
     , Typeable input, Typeable result
     )
  => witnesses -> (input -> Validation [String] result) -> DAG h result
with witnesses k = do
  aggregate <- combine witnesses
  using aggregate (`andThen` k)
