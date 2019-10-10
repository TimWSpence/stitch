{-# LANGUAGE GADTs, TypeOperators, FlexibleInstances, PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
  -- The signature for UArithOp is annoying

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Stitch.Op
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
--
-- Defines arithmetic and logical operators
--
----------------------------------------------------------------------------

module Language.Stitch.Op (

  -- * Arithmetic operators
  ArithOp(..), arithType, eqArithOp, eqArithOpB,

    -- ** Unchecked synonyms for arithmetic operators
  UArithOp, pattern UArithOp, uPlus, uMinus, uTimes, uDivide, uMod, uLess, uLessE,
  uGreater, uGreaterE, uEquals,

  ) where

import Language.Stitch.Data.Exists

import Language.Stitch.Util
import Language.Stitch.Type

import Type.Reflection ( typeRep )
import Data.Type.Equality
import Data.Maybe ( isJust )
import Data.Hashable

import Text.PrettyPrint.ANSI.Leijen

-- | An @ArithOp ty@ is an operator on numbers that produces a result
-- of type @ty@
data ArithOp ty where
  Plus, Minus, Times, Divide, Mod        :: ArithOp Int
  Less, LessE, Greater, GreaterE, Equals :: ArithOp Bool

-- | Extract the result type of an Op
arithType :: ArithOp ty -> TypeRep ty
arithType Plus     = typeRep
arithType Minus    = typeRep
arithType Times    = typeRep
arithType Divide   = typeRep
arithType Mod      = typeRep
arithType Less     = typeRep
arithType LessE    = typeRep
arithType Greater  = typeRep
arithType GreaterE = typeRep
arithType Equals   = typeRep

-- | 'UArithOp' ("unchecked 'ArithOp'") is an existential package for
-- an 'ArithOp'
type UArithOp = SingEx ArithOp

-- | Convenient pattern synonym to hide the underlying representation of UArithOp
pattern UArithOp op = SingEx op
{-# COMPLETE UArithOp #-}

uPlus, uMinus, uTimes, uDivide, uMod, uLess, uLessE, uGreater,
  uGreaterE, uEquals :: UArithOp
uPlus     = UArithOp Plus
uMinus    = UArithOp Minus
uTimes    = UArithOp Times
uDivide   = UArithOp Divide
uMod      = UArithOp Mod
uLess     = UArithOp Less
uLessE    = UArithOp LessE
uGreater  = UArithOp Greater
uGreaterE = UArithOp GreaterE
uEquals   = UArithOp Equals

-- | Compare two 'ArithOp's (potentially of different types) for equality
eqArithOp :: ArithOp ty1 -> ArithOp ty2 -> Maybe (ty1 :~: ty2)
eqArithOp Plus     Plus     = Just Refl
eqArithOp Minus    Minus    = Just Refl
eqArithOp Times    Times    = Just Refl
eqArithOp Divide   Divide   = Just Refl
eqArithOp Mod      Mod      = Just Refl
eqArithOp Less     Less     = Just Refl
eqArithOp LessE    LessE    = Just Refl
eqArithOp Greater  Greater  = Just Refl
eqArithOp GreaterE GreaterE = Just Refl
eqArithOp Equals   Equals   = Just Refl
eqArithOp _        _        = Nothing

-- | Compare two 'ArithOp's for uninformative equality
eqArithOpB :: ArithOp ty1 -> ArithOp ty2 -> Bool
eqArithOpB op1 op2 = isJust (eqArithOp op1 op2)

instance TestEquality ArithOp where
  testEquality = eqArithOp

instance Eq (ArithOp ty) where
  (==) = eqArithOpB

instance Hashable (ArithOp ty) where
  hashWithSalt s op = hashWithSalt s ctor_num
    where
      ctor_num :: Int
      ctor_num = case op of
        Plus     -> 0
        Minus    -> 1
        Times    -> 2
        Divide   -> 3
        Mod      -> 4
        Less     -> 5
        LessE    -> 6
        Greater  -> 7
        GreaterE -> 8
        Equals   -> 9

instance Eq UArithOp where
  UArithOp op1 == UArithOp op2 = op1 `eqArithOpB` op2

-------------------------------------
-- Pretty-printing

instance Pretty (ArithOp ty) where
  pretty Plus     = char '+'
  pretty Minus    = char '-'
  pretty Times    = char '*'
  pretty Divide   = char '/'
  pretty Mod      = char '%'
  pretty Less     = char '<'
  pretty LessE    = text "<="
  pretty Greater  = char '>'
  pretty GreaterE = text ">="
  pretty Equals   = text "=="

instance Show (ArithOp ty) where
  show = render . pretty

instance Pretty UArithOp where
  pretty (UArithOp op) = pretty op

instance Show UArithOp where
  show = render . pretty
