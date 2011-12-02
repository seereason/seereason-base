{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
module Ontology.Types.PF where

import Data.Logic.KnowledgeBase      (Proof(..))
import Data.Logic.Classes.FirstOrder (prettyFirstOrder, for_all)
import Data.Logic.Classes.Literal (prettyLit)
import Data.Logic.Classes.Pred (pApp)
import Data.Logic.Classes.Term (Term(var), prettyTerm)
import qualified Data.Text as T
import Ontology.Types.Formula        (AtomicPredicate(..), prettyAtomicPredicate, V(V),
                                      AtomicFunction, prettyAtomicFunction, prettyV, FormulaF, LiteralF, TermF)
import Ontology.Types                (Assertion, Subject, PredicateStyle(AsPredicate))
import Ontology.Types.Description   (Description, LinguisticHint(..), NounPhraseFragment(..))
import Ontology.Types.UserData      (UserData)
import Text.PrettyPrint (Doc)

type AtomicPredicatePF = AtomicPredicate Description
type AtomicFunctionPF = AtomicFunction Description
type LiteralPF = LiteralF Description
type FormulaPF = FormulaF Description
type TermPF = TermF Description
type AssertionPF = Assertion FormulaPF
type UserDataPF = UserData FormulaPF
type SubjectPF = Subject FormulaPF
--type ImplicativeNormalFormPF = ImplicativeNormalForm LiteralPF
type ProofPF = Proof LiteralPF

prettyLitPF :: Int -> LiteralPF -> Doc
prettyLitPF = prettyLit prettyV (prettyAtomicPredicate AsPredicate) prettyAtomicFunction

prettyFormulaPF :: Int -> FormulaPF -> Doc
prettyFormulaPF = prettyFirstOrder prettyV (prettyAtomicPredicate AsPredicate) prettyAtomicFunction

prettyTermPF :: TermPF -> Doc
prettyTermPF = prettyTerm prettyV prettyAtomicFunction

-- |Quick way to turn a string into a description.
desc :: Int -> String -> AtomicPredicatePF
desc a s = Description a (NounPhrase, [T (T.pack s)])

-- |Arity 1 description
desc1 :: String -> AtomicPredicatePF
desc1 = desc 1

-- |Arity 2 description
desc2 :: String -> AtomicPredicatePF
desc2 = desc 2

defaultFormula :: FormulaPF
defaultFormula = (for_all (V "x") (pApp (Empty) [var (V "x")])) :: FormulaPF
