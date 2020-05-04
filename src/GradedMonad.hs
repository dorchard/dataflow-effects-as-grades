{-# LANGUAGE KindSignatures, TypeFamilies, ConstraintKinds, PolyKinds, MultiParamTypeClasses #-}

-- Similar to what is provided in the `effect-monad`
-- package (https://hackage.haskell.org/package/effect-monad)
-- but with some naming that is convenient for our purposes.

module GradedMonad where

import Prelude hiding (Monad(..))
import GHC.Exts ( Constraint )
import GHC.Types

-- Graded monad structure
class GradedMonad (m :: d -> Type -> Type) where

   -- Pomonoid effect algebra
   -- Identity elemement - effect of a trivially effectful computation
   type Unit m :: d
   -- Monoid multiplication - combining effects of two subcomputations
   type Seq m (r :: d) (s :: d) :: d

   -- Partial order as a relation
   type Sub m (r :: d) (s :: d) :: Constraint

   -- 'Inv' provides a way to give instances of 'GradedMonad' their
   -- own constraints for '>>=' if needed.
   type Inv m (r :: d) (s :: d) :: Constraint
   type Inv m r s = ()

   {-| Graded version of 'return'. Annotated with the 'Unit m' effect,
    denoting pure compuation -}
   return :: a -> m (Unit m) a

   {-| Graded version of '>>=' (bind). Combines
    two effect annotations 'f' and 'g' on its parameter computations via 'Seq' -}

   (>>=) :: m r a -> (a -> m s b) -> m (Seq m r s) b

   (>>) :: m r a -> m s b -> m (Seq m r s) b
   x >> y = x >>= (\_ -> y)

   {-| Graded counterpart to pre-ordering -}
   sub :: Sub m r s => m r a -> m s a

fail = undefined
