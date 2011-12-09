{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}
module Ontology.Types.GenI
    ( LSubject(..)
    ) where

import Data.Generics (Typeable, Data)
import Ontology.Types.Assertion (Assertion)
import Ontology.Types.Subject (SubjectId, SubjectNode, SubjectTuple)

-- | A version of Ontology.Types.Subject without sets, because sets
-- and maps cause the generic json functions to barf.
data LSubject formula
    = LSubject
      { subjectEquivalenceSet :: [Assertion formula]
      , subjectIds :: [SubjectNode]
      , subjectDefinitionMap :: [(SubjectId, [SubjectTuple formula])]
      , subjectArity :: Maybe Int }
    deriving (Eq, Ord, Typeable, Data)
