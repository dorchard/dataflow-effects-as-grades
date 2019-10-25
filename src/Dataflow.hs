-- By Andrej Ivaskovic, Alan Mycroft, and Dominic Orchard
-- Copyright 2019

{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dataflow where

import Prelude hiding (Monad(..))
import qualified Prelude as M
import GradedMonad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.Type.Set hiding (Remove, Set)
import GHC.TypeLits
import Data.Kind
import Data.Constraint.Forall

-- Default meaning of if-then-else needs reinstating due to
-- RebindableSyntax
ifThenElse True x _ = x
ifThenElse False _ y = y

-- Four-variable graded monad representing transfer functions as (what
-- will be functional) relations

data MultiState (func :: [Symbol] -> [Symbol] -> Constraint) x =
  MultiState { unMultiState :: StateT Int (StateT Int (StateT Int (StateT Int Identity))) x }

-- The operations just wrap the underlying monad, packing and unpacking
-- the data type wrapped via its constructor and deconstructor.
instance GradedMonad MultiState where
   type Unit MultiState     = Id
   type Seq  MultiState r s = r :|> s
   type Sub  MultiState r s = PointwiseSub r s

   return x = MultiState $ M.return x
   (MultiState x) >>= k = MultiState ((M.>>=) x (unMultiState . k))

   sub (MultiState x) = MultiState x

-- Identity function
class Id dIn dOut | dIn -> dOut
instance Id d d

-- Function composition
class (:|>) f g dIn dOut
instance (f dIn dMid, g dMid dOut) => (:|>) g f dIn dOut

---- LVA example

{-| Symbol comparison -}
type instance Cmp (v :: Symbol) (u :: Symbol) = CmpSymbol v u

-- Gen / get

class Gen (v :: Symbol) dIn dOut | v dIn -> dOut
instance Gen v dIn (v ': dIn) -- add `v` onto the incoming set `dIn`

getX :: MultiState (Gen "x") Int
getX = MultiState get

getY :: MultiState (Gen "y") Int
getY = MultiState (lift get)

getZ :: MultiState (Gen "z") Int
getZ = MultiState (lift (lift get))

getR :: MultiState (Gen "r") Int
getR = MultiState (lift (lift get))

-- Kill / put

class Kill (v :: Symbol) (dIn :: [Symbol]) (dOut :: [Symbol]) | v dIn -> dOut
instance (dIn :\ v) ~ dOut => Kill v dIn dOut -- remove `v` from `dIn` to get `dOut`

putX :: Int -> MultiState (Kill "x") ()
putX x = MultiState (put x)

putY :: Int -> MultiState (Kill "y") ()
putY x = MultiState (lift (put x))

putZ :: Int -> MultiState (Kill "z") ()
putZ x = MultiState (lift (lift (put x)))

putR :: Int -> MultiState (Kill "r") ()
putR x = MultiState (lift (lift (lift (put x))))

--- Reify the constraint at a particular program point applying the
--boundary information (empty set)

data Set s = Set -- proxy with phantom type parameter

atProgramPoint :: r '[] dOut => MultiState r x -> Set (AsSet dOut)
atProgramPoint (MultiState _) = Set

-- Inverse example (kill :|> gen is the identity on the dataflow values)
inverseEample = do
  putX 42
  x <- getX
  return ()

-- Example 3

example3 g  = do
  x <- getX
  y <- getY
  putZ (x + y)
  g

-- Force calculation of the type by passing in the "empty set"

-- Get's inferred as Set '["x", "y"]
example3' = atProgramPoint (example3 (return ()))

-- Another example.

exampleAlt = do
  (x :: Int) <- getX
  y <- getY
  alt (x >= y) getZ getR

-- Get's inferred as Set '["r", "x", "y", "z"]
exampleAlt' = atProgramPoint exampleAlt

-------------------------------------------------
-- Further things here for doing `sub`

-- Partial order representation (specialised for our four variable
-- example)

class Member' (a :: Symbol) (as :: [Symbol]) (r :: Bool) | a as -> r
instance Member' x '[] False
instance Member' x (x ': xs) True
instance Member' x xs b => Member' x (y ': xs) b

class Subset' (a :: [Symbol]) (b :: [Symbol]) (r :: Bool) | a b -> r
instance Subset' '[] xs True
instance (Member' x ys True, Subset' xs ys r) => Subset' (x ': xs) ys r

class Subsets (xs :: [Symbol])
instance Subsets '[]
instance Subsets '["x"]
instance Subsets '["y"]
instance Subsets '["z"]
instance Subsets '["r"]
instance Subsets '["x", "y"]
instance Subsets '["x", "z"]
instance Subsets '["x", "r"]
instance Subsets '["y", "z"]
instance Subsets '["y", "r"]
instance Subsets '["r", "z"]
instance Subsets '["x", "y", "z"]
instance Subsets '["x", "y", "r"]
instance Subsets '["x", "z", "r"]
instance Subsets '["y", "z", "r"]
instance Subsets '["x", "y", "z", "r"]

class PointwiseSub (r :: [Symbol] -> [Symbol] -> Constraint) (s :: [Symbol] -> [Symbol] -> Constraint)
instance (Subsets d, r d dOut1, s d dOut2, Subset' dOut1 dOut2 True) => PointwiseSub r s

-- Alternation representation (if used instead of partial order)

class Alt (f :: k -> k -> Constraint) (g :: k -> k -> Constraint) (dIn :: k) (dOut :: k)
instance (f dIn fout, g dIn gout, Append fout gout dOut) => Alt f g dIn dOut

alt :: Bool -> MultiState r x -> MultiState s x
           -> MultiState (Alt r s) x
alt True (MultiState x) _  = MultiState x
alt False _ (MultiState y) = MultiState y

class Append (xs :: [k]) (ys :: [k]) (zs :: [k]) | xs ys -> zs
instance Append '[] xs xs
instance Append xs ys zs => Append (x ': xs) ys (x ': zs)