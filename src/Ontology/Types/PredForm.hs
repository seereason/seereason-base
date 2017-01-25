{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Ontology.Types.PredForm
    ( -- * PredForm
      PredForm(PredForm)
    , foldPred
    , makePred
    ) where

import Data.Data (Data(..))
import Data.Logic.ATP.Apply (HasApply(TermOf, PredOf), pApp)
import Data.Logic.ATP.Equate (HasEquate, foldEquate)
import Data.Logic.ATP.Formulas (AtomOf)
import Data.Logic.ATP.Quantified (IsQuantified(foldQuantified))
import Data.Logic.ATP.Term (IsTerm(vt, TVarOf), variants)
import Data.Monoid ((<>))
import Data.SafeCopy -- (base, extension, deriveSafeCopy)
import Data.String (IsString(fromString))
import Data.Typeable (Typeable)
import Ontology.Arity (HasArity(arity))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text, hang)

-- |A PredForm is a wrapper around a formula that indicates that
-- the formula is of the form Apply p [term].
newtype PredForm formula = PredForm formula
    deriving (Eq, Ord, Data, Typeable, Read, Show)

instance Pretty formula => Pretty (PredForm formula) where
    pPrint (PredForm fm) = text "(PredForm " <> pPrint fm <> text ")"

-- ^ This function is used to access the predicate in a PredForm.
-- Note that the type "a" might be a function such as "[term] -> b",
-- which means you can simulate the pApp function.
foldPred :: (atom ~ AtomOf formula, p ~ PredOf atom,
             IsQuantified formula, HasEquate atom) => (p -> a) -> PredForm formula -> a
foldPred fn (PredForm form) =
    foldQuantified qu co ne tf at form
    where
      at = foldEquate (\ _ _ -> error "foldPred") (\ p _ -> fn p)
      tf = error "foldPred" -- fn . fromBool
      qu = error "foldPred"
      ne = error "foldPred"
      co = error "foldPred"

-- |Create a PredForm from an atomic predicate and some generated terms.
makePred :: (atom ~ AtomOf formula, v ~ TVarOf term, p ~ PredOf atom, term ~ TermOf atom,
             IsQuantified formula, HasEquate atom, IsTerm term, HasArity p) => p -> PredForm formula
makePred p = PredForm (pApp p ts)
    where ts = case arity p of
                 Nothing -> error "makePred: Fixed arity expected"
                 Just n -> take n (map vt (variants (fromString "x")))

instance (atom ~ AtomOf formula, p ~ PredOf atom,
          IsQuantified formula, HasEquate atom, HasArity p) => HasArity (PredForm formula) where
    arity = foldPred arity

$(deriveSafeCopy 1 'base ''PredForm)
