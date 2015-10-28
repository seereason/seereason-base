{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PackageImports, ScopedTypeVariables,
             StandaloneDeriving, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-orphans #-}
module Ontology.Types.Formula
    ( module FOL
    , module Ontology.Types.Formula.AtomicPredicate
    , module Ontology.Types.Formula.AtomicFunction
    , FormulaF
    , LiteralF
    , AtomF
    , TermF
    ) where

import Data.Generics (Data)
import qualified Data.Logic.Types.FirstOrder as N
import Data.Logic.Types.FirstOrderPublic as P
import FOL (V(V))
import Lib (Marked)
import Ontology.Types.Formula.AtomicPredicate
import Ontology.Types.Formula.AtomicFunction
import Pretty (Pretty)
import Prop (IsAtom, Literal)

type FormulaF description = Marked Public (N.NFormula V (AtomicPredicate description) (AtomicFunction description V))
type LiteralF description = Marked Literal (FormulaF description) -- (N.NFormula V (AtomicPredicate description) (AtomicFunction description V))
type TermF description = N.NTerm V (AtomicFunction description V)
type AtomF description = N.NPredicate (AtomicPredicate description) (TermF description)

{-
instance (Data description, Ord description, Show description, Pretty description
         ) => HasPredicate (AtomF description) (AtomicPredicate description) (TermF description) where
 -- applyPredicate :: predicate -> [term] -> atom
    applyPredicate = N.Apply
 -- foldPredicate :: (predicate -> [term] -> r) -> atom -> r
    foldPredicate f (N.Apply p ts) = f p ts
    foldPredicate _ (N.Equal _ _) = error "foldPredicate - found Equate"
-}
