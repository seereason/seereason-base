{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Ontology.Types.PredForm
    ( -- * PredForm
      PredForm(PredForm)
    , foldPred
    , makePred
    ) where

import Data.Data (Data(..))
import FOL (pApp)
import Ontology.Arity (HasArity(arity))
import Formulas (AtomOf, fromBool)
import FOL (HasApply(TermOf, PredOf), HasApplyAndEquate, foldEquate)
import FOL (IsQuantified(VarOf, foldQuantified))
import FOL (IsTerm(vt))
import FOL (variants)
import Data.SafeCopy -- (base, extension, deriveSafeCopy)
import Data.String (IsString(fromString))
import Data.Typeable (Typeable)

-- |A PredForm is a wrapper around a formula that indicates that
-- the formula is of the form Apply p [term].
newtype PredForm formula = PredForm formula
    deriving (Eq, Ord, Data, Typeable, Read, Show)

-- ^ This function is used to access the predicate in a PredForm.
-- Note that the type "a" might be a function such as "[term] -> b",
-- which means you can simulate the pApp function.
foldPred :: (atom ~ AtomOf formula, p ~ PredOf atom,
             IsQuantified formula, HasApplyAndEquate atom) => (p -> a) -> PredForm formula -> a
foldPred fn (PredForm form) =
    foldQuantified qu co ne tf at form
    where
      at = foldEquate (\ _ _ -> error "foldPred") (\ p _ -> fn p)
      tf = fn . fromBool
      qu = error "foldPred"
      ne = error "foldPred"
      co = error "foldPred"

-- |Create a PredForm from an atomic predicate and some generated terms.
makePred :: (atom ~ AtomOf formula, v ~ VarOf formula, p ~ PredOf atom, term ~ TermOf atom,
             IsQuantified formula, HasApplyAndEquate atom, IsTerm term, HasArity p) => p -> PredForm formula
makePred p = PredForm (pApp p ts)
    where ts = case arity p of
                 Nothing -> error "makePred: Fixed arity expected"
                 Just n -> take n (map vt (variants (fromString "x")))

instance (atom ~ AtomOf formula, p ~ PredOf atom,
          IsQuantified formula, HasApplyAndEquate atom, HasArity p) => HasArity (PredForm formula) where
    arity = foldPred arity

$(deriveSafeCopy 1 'base ''PredForm)
