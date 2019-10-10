{-# LANGUAGE DataKinds, TypeOperators, PolyKinds, StandaloneDeriving,
             GADTs, RankNTypes, FlexibleInstances, PatternSynonyms,
             ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes,
             ViewPatterns, TypeInType, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}              -- this module declares Type instances
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
   -- the pattern signature for Ty is annoying

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Stitch.Type
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
--
-- Defines types
--
----------------------------------------------------------------------------

module Language.Stitch.Type (
    Ty, pattern Ty, mkTy, Type, TypeRep, typeRep, eqTypeRep
  , extractArgType, extractResType
  , pattern (:->), pattern (:@:), isTypeRep
  ) where

import Language.Stitch.Util

import Text.PrettyPrint.ANSI.Leijen
import Data.Kind
import Type.Reflection
import Language.Stitch.Data.Singletons
import Language.Stitch.Data.Exists

import Data.Maybe ( isJust )

-- | An existential package for a TypeRep of kind Type
type Ty = Ex (TypeRep :: Type -> Type)

-- | Convenient pattern synonym
pattern Ty t = Ex t
{-# COMPLETE Ty #-}

-- | Produce a Ty for a given type (which must be given using type application)
mkTy :: forall (a :: Type). Typeable a => Ty
mkTy = Ty (typeRep @a)

instance Eq Ty where
  Ty a == Ty b = isJust (a `eqTypeRep` b)

-- Some convenient synonyms

-- This one is necessary because TypeRep works with potentially levity-polymorphic
-- function types. We want to restrict just to Type -- Stitch doesn't support levity
-- polymorphism, of course.
pattern (:->) :: forall (fun :: Type). ()
              => forall (arg :: Type) (res :: Type). (fun ~ (arg -> res))
              => TypeRep arg -> TypeRep res
              -> TypeRep fun
pattern arg :-> res <- (checkFun -> FunOnTypes arg res)
  where
    arg :-> res = arg `Fun` res

data CheckFun fun where
  FunOnTypes :: forall (arg :: Type) (res :: Type).
                TypeRep arg -> TypeRep res -> CheckFun (arg -> res)
  OtherType :: CheckFun fun

checkFun :: TypeRep fun -> CheckFun fun
checkFun (arg `Fun` res)
  | Just HRefl <- typeRepKind arg `eqTypeRep` typeRep @Type
  , Just HRefl <- typeRepKind res `eqTypeRep` typeRep @Type
  = FunOnTypes arg res
checkFun _other = OtherType

-- The extra pattern matches in these functions make me sad. They are due to a
-- GHC infelicity.
extractArgType :: forall (arg :: Type) (res :: Type).
                  TypeRep (arg -> res) -> TypeRep arg
extractArgType (arg `Fun` _)       = arg
extractArgType (App (App _ arg) _) = arg

extractResType :: forall (arg :: Type) (res :: Type).
                  TypeRep (arg -> res) -> TypeRep res
extractResType (_ `Fun` res) = res
extractResType (App _ res)   = res

pattern (:@:) :: forall k_res (app :: k_res). ()
              => forall k_arg (fun :: k_arg -> k_res) (arg :: k_arg). (app ~ fun arg)
              => TypeRep fun -> TypeRep arg
              -> TypeRep app
pattern f :@: a = f `App` a

-- Convenient view pattern
isTypeRep :: forall a b. Typeable a => TypeRep b -> Maybe (a :~~: b)
isTypeRep = eqTypeRep (typeRep @a)

-----------------------------------------
-- Pretty-printing

instance Pretty Ty where
  pretty (Ty ty) = pretty_ty topPrec ty

instance Pretty (TypeRep ty) where
  pretty = pretty_ty topPrec

arrowLeftPrec, arrowRightPrec, arrowPrec :: Prec
arrowLeftPrec  = 5
arrowRightPrec = 4.9
arrowPrec      = 5

pretty_ty :: Prec -> TypeRep ty -> Doc
pretty_ty prec (arg `Fun` res) = maybeParens (prec >= arrowPrec) $
                                 hsep [ pretty_ty arrowLeftPrec arg
                                      , text "->"
                                      , pretty_ty arrowRightPrec res ]
pretty_ty _    other           = text (show other)

-----------------------------------------
-- Sing instance for types

instance SingKind Type where
  type Sing = TypeRep

  fromSing _ = error "no term-level Types"

instance Typeable a => SingI (a :: Type) where
  sing = typeRep
