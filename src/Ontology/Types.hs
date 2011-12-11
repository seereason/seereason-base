{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wall #-}
-- |An /Assertion/ is a record containing a formula and some
-- associated information for book keeping in the database:
-- 
--  * a unique identifier,
-- 
--  * the user who asserted it (created the assertion),
-- 
--  * when it was created,
-- 
--  * a state variable - is it private, public, is it being edited
-- 
module Ontology.Types
    ( module Ontology.Types.Assertion
    , module Ontology.Types.Belief
    , module Ontology.Types.DocumentId
    -- , module Ontology.Types.Function
    , module Ontology.Types.PredForm
    , module Ontology.Types.Subject
    , module Ontology.Types.Theorem
    , DB_Error(..)
    ) where

import Ontology.Types.Assertion
import Ontology.Types.Belief
import Ontology.Types.DocumentId
-- import Ontology.Types.Function
import Ontology.Types.PredForm
import Ontology.Types.Subject
import Ontology.Types.Theorem

import Data.Data (Data(..))
import Data.Typeable (Typeable)

data DB_Error formula
    = AssertionAlreadyStored (Assertion formula)
    | InvalidAssertionId AssertionId
    | InvalidRevision AssertionId [String]
    | FailureToCreate formula [String]
    | FailureToReplace AssertionId
    | FailureToClose [AssertionId]
    | FailureToDelete AssertionId
    | FailureToCombine AssertionId
      deriving (Eq, Ord, Data, Typeable)
