{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}
module Ontology.Types.GenI
    ( LSubject(..)
    ) where

import Data.Generics (Typeable, Data)
import qualified Ontology.Types as O

-- | A version of Ontology.Types.Subject without sets, because sets
-- and maps cause the generic json functions to barf.
data LSubject formula
    = LSubject
      { subjectEquivalenceSet :: [O.Assertion formula]
      , subjectIds :: [O.SubjectNode]
      , subjectDefinitionMap :: [(O.SubjectId, [O.SubjectTuple formula])]
      , subjectArity :: Maybe Int }
    deriving (Eq, Ord, Typeable, Data)
