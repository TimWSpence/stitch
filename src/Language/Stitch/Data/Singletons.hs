{-# LANGUAGE RankNTypes, TypeInType, TypeFamilies, TypeOperators,
             GADTs, TypeApplications,
             ScopedTypeVariables, InstanceSigs, StandaloneDeriving,
             FlexibleContexts, UndecidableInstances, FlexibleInstances #-}

module Language.Stitch.Data.Singletons where

import Data.Kind
import Language.Stitch.Data.Vec
import Language.Stitch.Data.Nat

class SingKind k where
  -- It's a bit cleaner than the original approach to
  -- use a type family than a data family: allows us
  -- to label TypeRep as a singleton.
  type Sing :: k -> Type

  fromSing :: Sing (a :: k) -> k

-- implicit singletons
class SingI (a :: k) where
  sing :: Sing a

------------------------------
-- Singleton vectors

data SVec :: forall (a :: Type) (n :: Nat). Vec a n -> Type where
  SVNil :: SVec VNil
  (:%>) :: Sing a -> Sing as -> SVec (a :> as)
infixr 5 :%>

deriving instance ShowSingVec v => Show (SVec v)

instance SingKind a => SingKind (Vec a n) where
  type Sing = SVec

  fromSing SVNil     = VNil
  fromSing (h :%> t) = fromSing h :> fromSing t

instance SingI VNil where
  sing = SVNil
instance (SingI h, SingI t) => SingI (h :> t) where
  sing = sing :%> sing

-- | Make a Show constraint for a singleton vector
type family ShowSingVec (v :: Vec a n) :: Constraint where
  ShowSingVec VNil      = ()
  ShowSingVec (x :> xs) = (Show (Sing x), ShowSingVec xs)

------------------------------
-- Singleton Nats

data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

deriving instance Show (SNat n)

instance SingKind Nat where
  type Sing = SNat

  fromSing SZero     = Zero
  fromSing (SSucc n) = Succ (fromSing n)

instance SingI Zero where
  sing = SZero
instance SingI n => SingI (Succ n) where
  sing = SSucc sing

snatToInt :: SNat n -> Int
snatToInt SZero     = 0
snatToInt (SSucc n) = 1 + snatToInt n
