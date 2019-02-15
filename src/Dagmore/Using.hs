{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | When we're dealing with sets of witnesses, each witness has a different
-- type. If we want to return some number of differently-typed values to a
-- user, we typically need an HList-like structure. However, there's a
-- problem: HLists are ugly.
--
-- In order to remedy this, we can use tuples instead, but this also isn't
-- straightforward: GHC's tuple types are all totally unrelated: a three-tuple
-- isn't syntactic sugar on top of nested two-tuples like Idris or PureScript,
-- so we have to get creative. Thankfully, in TemplateHaskell, the shapes of
-- tuples /are/ far more regular, so we can write this unearthly mess below!
module Dagmore.Using where

import Dagmore.TH
import Data.Foldable       (foldl')
import Data.Functor        ((<&>))
import Data.List.NonEmpty  (NonEmpty (..))
import Language.Haskell.TH
import Prelude             hiding (head, tail)

-- | Apply a typeable constraint to every variable name.
constraints :: NonEmpty Name -> [Type]
constraints (head :| tail)
  = fmap typeable (f : head : tail)

-- | The instance head for a given set of names' instances!
instanceHead :: NonEmpty Name -> Type
instanceHead names = ConT (mkName "Using")
      `AppT` region
      `AppT` VarT (mkName "f")
      `AppT` witnesses names
      `AppT` outputsF  names

-- | Apply a function to every element in a tuple.
outputB :: NonEmpty Name -> Body
outputB = NormalB . TupE . foldMap (pure . function . VarE)

-- | Make a tuple of f-applied values.
outputsF :: NonEmpty Name -> Type
outputsF xs = foldl' go (TupleT (length xs)) xs
  where
    go :: Type -> Name -> Type
    go constructor = AppT constructor . AppT (VarT f) . VarT

-- | Build the instances for 'Dagmore.Using'.
instances :: Q [Dec]
instances = do
  head :| tail <- fresh

  pure $ [1 .. 61 :: Int] <&> \size -> do
    let names = head :| take size tail

    InstanceD Nothing (constraints names) (instanceHead names)
      [ FunD (mkName "using'")
          [ Clause [ VarP (mkName "f"), inputP names ] (outputB names) []
          ]
      ]
