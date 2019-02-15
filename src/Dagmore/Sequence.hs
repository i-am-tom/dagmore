{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | This @TemplateHaskell@ mess provides the "generalisation" of
-- tuple-sequencing. We don't necessarily want to force the user to use the
-- 'Applicative' instance, so the API's core function requires a "zipping"
-- function to dictate how we traverse the tuple.
module Dagmore.Sequence where

import           Dagmore.TH
import qualified Dagmore.Using       as Using
import           Language.Haskell.TH hiding (match)
import           Data.Foldable       (foldl', foldr1)
import           Data.Functor        ((<&>))
import           Data.List.NonEmpty  (NonEmpty (..))
import           Prelude             hiding (head, sequence, tail)

-- | Create the expression for the 'Dagless.combineWith' implementation.
sequence :: NonEmpty Name -> Exp
sequence xs
  = VarE (mkName "fmap")
      `AppE` LamE [nested xs] (unnested xs)
      `AppE` apply xs
  where
    nested :: NonEmpty Name -> Pat
    nested = foldr1 (\x y -> TupP [ x, y ]) . fmap VarP

    unnested :: NonEmpty Name -> Exp
    unnested = TupE . fmap VarE . foldMap pure 

    apply :: NonEmpty Name -> Exp
    apply = foldr1 (\x y -> VarE f `AppE` x `AppE` y) . fmap VarE

-- | Create a tuple of the given variables.
output :: NonEmpty Name -> Type
output xs = foldl' go (TupleT (length xs)) xs
  where
    go :: Type -> Name -> Type
    go constructor = AppT constructor . VarT

-- | The @Functor f@ constraint.
functor :: Type
functor = ConT (mkName "Functor") `AppT` VarT f

-- | Build the 'Dagmore.Sequence' instances.
instances :: Q [Dec]
instances = do
  head :| tail <- fresh

  pure $ [1 .. 61] <&> \size -> do
    let names = head :| take size tail

        choice = ConT (mkName "Sequence")
          `AppT` VarT (mkName "f")
          `AppT` region
          `AppT` witnesses names
          `AppT` output names

        body = VarE (mkName "using")
          `AppE` VarE (mkName "xs")
          `AppE` LamE [ inputP names ] (sequence names)

        constraints
          = functor
          : Using.instanceHead names
          : foldMap (pure . typeable) names

    InstanceD Nothing constraints choice
      [ FunD (mkName "combineWith")
          [ Clause [ VarP (mkName "f"), VarP (mkName "xs") ] (NormalB body) []
          ]
      ]
