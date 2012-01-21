{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PackageImports, ScopedTypeVariables,
             StandaloneDeriving, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-orphans #-}
module Ontology.Types.Formula
    ( module Ontology.Types.Formula.V
    , module Ontology.Types.Formula.AtomicPredicate
    , module Ontology.Types.Formula.AtomicFunction
    , FormulaF
    , LiteralF
    , TermF
    ) where

import qualified Data.Logic.Types.FirstOrder as N
import qualified Data.Logic.Types.FirstOrderPublic as P
import Ontology.Types.Formula.V
import Ontology.Types.Formula.AtomicPredicate
import Ontology.Types.Formula.AtomicFunction

type FormulaF description = P.Formula V (AtomicPredicate description) (AtomicFunction description V)
type LiteralF description = N.Formula V (AtomicPredicate description) (AtomicFunction description V)
type TermF description = N.PTerm V (AtomicFunction description V)
