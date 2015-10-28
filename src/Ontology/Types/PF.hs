{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
module Ontology.Types.PF where

import Data.Logic.KnowledgeBase (Proof(..))
import qualified Data.Text as T
import FOL (pApp, for_all, IsTerm(vt))
import Ontology.Types.Formula (AtomicPredicate(..), V(V), AtomicFunction(..), FormulaF, LiteralF, AtomF, TermF)
import Ontology.Types (Assertion, Subject)
import Ontology.Types.Description (Description(Description'), LinguisticHint(..), NounPhraseFragment(..))
import Ontology.Types.UserData (UserData)

type AtomicPredicatePF = AtomicPredicate Description
type AtomicFunctionPF = AtomicFunction Description V
type LiteralPF = LiteralF Description
type FormulaPF = FormulaF Description
type AtomPF = AtomF Description
type TermPF = TermF Description
type AssertionPF = Assertion FormulaPF
type UserDataPF = UserData FormulaPF
type SubjectPF = Subject FormulaPF
--type ImplicativeNormalFormPF = ImplicativeNormalForm LiteralPF
type ProofPF = Proof LiteralPF

{-
prettyLitPF :: Int -> LiteralPF -> Doc
prettyLitPF = prettyLit pa prettyV
    where pa = prettyAtomEq prettyV (prettyAtomicPredicate AsPredicate) prettyAtomicFunction

prettyFormulaPF :: Int -> FormulaPF -> Doc
prettyFormulaPF = prettyFirstOrder pa prettyV
    where pa = prettyAtomEq prettyV (prettyAtomicPredicate AsPredicate) prettyAtomicFunction

-- instance Pretty FormulaPF where
--     pPrint = prettyFormulaPF 0

prettyTermPF :: TermPF -> Doc
prettyTermPF = prettyTerm prettyV prettyAtomicFunction

instance Pretty TermPF where
    pPrint = prettyTermPF
-}

-- |Quick way to turn a string into a description.
desc :: Int -> String -> AtomicPredicatePF
desc a s = Described a (Description' NounPhrase [T (T.pack s)])

-- |Arity 1 description
desc1 :: String -> AtomicPredicatePF
desc1 = desc 1

-- |Arity 2 description
desc2 :: String -> AtomicPredicatePF
desc2 = desc 2

-- for_all :: IsQuantified formula atom v => v -> formula -> formula
-- pApp :: (IsFormula formula atom, IsAtom atom predicate term) => predicate -> [term] -> formula
-- IsTerm term v function => vt :: v -> term

defaultFormula :: FormulaPF
defaultFormula = (for_all (V "x") (pApp Empty [vt (V "x")]))
