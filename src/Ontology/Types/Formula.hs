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

import Data.Generics (Data)
import Data.Logic.Classes.Constants (Boolean(..))
import Data.Logic.Classes.FirstOrder (pApp0)
import qualified Data.Logic.Types.FirstOrder as N
import qualified Data.Logic.Types.FirstOrderPublic as P
import Ontology.Types.Formula.V
import Ontology.Types.Formula.AtomicPredicate
import Ontology.Types.Formula.AtomicFunction

type FormulaF description = P.Formula V (AtomicPredicate description) (AtomicFunction description)
type LiteralF description = N.Formula V (AtomicPredicate description) (AtomicFunction description)
type TermF description = N.PTerm V (AtomicFunction description)

instance (Ord description, Data description, Show description) => Boolean (FormulaF description) where
    fromBool x = pApp0 (fromBool x)

instance (Ord description, Show description) => Boolean (LiteralF description) where
    fromBool x = pApp0 (fromBool x)
