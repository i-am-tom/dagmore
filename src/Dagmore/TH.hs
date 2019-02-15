-- | I've written more @TemplateHaskell@ for this library than for anything
-- ever. Forgive me, Padre. In order to minimise the exposure, this module
-- contains the "common pieces" for the two TH modules.
module Dagmore.TH where

import Data.Foldable       (foldl')
import Data.List.NonEmpty  (NonEmpty (..))
import Language.Haskell.TH

-- | We'll use this type for the "region of memory" on the 'Dagmore.Witness'
-- values. Of course, it's not really a region of memory: this is the analogy
-- used for the 'Control.Monad.ST' type, from which I got the trick.
region :: Type
region = VarT (mkName "h")

-- | A variable named @f@.
f :: Name
f = mkName "f"

-- | Apply a function called @f@ to an expression.
function :: Exp -> Exp
function = AppE (VarE f)

-- | A pattern-match for a tuple with the given variable names.
inputP :: NonEmpty Name -> Pat
inputP = TupP . foldMap (pure . VarP)

-- | Produce witnesses for the given type names.
witnesses :: NonEmpty Name -> Type
witnesses xs = foldl' go (TupleT (length xs)) xs
  where
    witness :: Type -> Type
    witness = AppT (ConT (mkName "Witness") `AppT` region)

    go :: Type -> Name -> Type
    go constructor = AppT constructor . witness . VarT

-- | Apply a 'Type.Reflection.Typeable' constraint to a variable name.
typeable :: Name -> Type
typeable = AppT (ConT (mkName "Typeable")) . VarT

-- | Produce some fresh variables.
fresh :: Q (NonEmpty Name)
fresh = traverse (\_ -> newName "t") (1 :| [2 .. 62 :: Int])
