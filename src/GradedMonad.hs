{-# LANGUAGE KindSignatures, TypeFamilies, ConstraintKinds, PolyKinds, MultiParamTypeClasses #-}

-- Taken from the `effect-monad` package.

module GradedMonad where

import Prelude hiding (Monad(..))
import GHC.Exts ( Constraint )
import GHC.Types

{-| Specifies "parametric effect monads" which are essentially monads but
     annotated by a type-level monoid formed by 'Plus' and 'Unit' -}
class GradedMonad (m :: d -> Type -> Type) where

   {-| Effect of a trivially effectful computation |-}
   type Unit m :: d
   {-| Cominbing effects of two subcomputations |-}
   type Seq m (r :: d) (s :: d) :: d

   {-| 'Inv' provides a way to give instances of 'Effect' their own constraints for '>>=' -}
   type Inv m (r :: d) (s :: d) :: Constraint
   type Inv m r s = ()

   type Sub m (r :: d) (s :: d) :: Constraint

   {-| Effect-parameterised version of 'return'. Annotated with the 'Unit m' effect,
    denoting pure compuation -}
   return :: a -> m (Unit m) a

   {-| Effect-parameterise version of '>>=' (bind). Combines
    two effect annotations 'f' and 'g' on its parameter computations into 'Plus' -}

   (>>=) :: m r a -> (a -> m s b) -> m (Seq m r s) b

   (>>) :: m r a -> m s b -> m (Seq m r s) b
   x >> y = x >>= (\_ -> y)

   sub :: Sub m r s => m r a -> m s a

fail = undefined