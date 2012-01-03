{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances, UndecidableInstances #-}
module Ontology.Types.Formula.AtomicFunction
    ( AtomicFunction(..)
    , prettyAtomicFunction
    ) where

import Data.Data (Data)
import Data.Logic.Classes.Arity (Arity(arity))
import Data.Logic.Classes.Skolem (Skolem(..))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)
import Ontology.Types (prettySubjectId, PredicateStyle(AsFunction))
import Ontology.Types.Formula.AtomicPredicate (AtomicPredicate(..), prettyAtomicPredicate, prettyNumberLit)
import Text.PrettyPrint (Doc, text)

-- |The atomic function used as a parameter to the
-- 'Logic.Predicate.Formula' type.
data AtomicFunction description
    = Function (AtomicPredicate description)
    -- ^ If the argument is an n-ary predicate, this is an (n-1)-ary
    -- function.  The pairs of the function are formed by decomposing
    -- each n-tuple in the subject to an (n-1)-tuple and a singleton.
    -- The value of the function is the union of the singletons for a
    -- given (n-1)-tuple.  So a unary predicate like Somebody UserId
    -- turns in a constant function whose value is that user.
    | Skolem Int                    -- ^ A temporary value used by the automatic theorem prover
    deriving (Eq, Ord, Data, Typeable, Show)

instance Skolem (AtomicFunction description) where
    toSkolem = Skolem
    fromSkolem (Skolem n) = Just n
    fromSkolem _ = Nothing

prettyAtomicFunction :: (Eq description, Ord description, Show description) => AtomicFunction description -> Doc
prettyAtomicFunction x =
    case x of
      Function (Reference _ ident) -> prettySubjectId AsFunction ident
      Function (NumberLit d) -> prettyNumberLit d
      Function p -> prettyAtomicPredicate AsFunction p
      _ -> text . show $ x

instance (Ord description, Show description) => Arity (AtomicFunction description) where
    arity (Function p) = maybe Nothing (\ n -> Just (n - 1)) (arity p)
    arity (Skolem _) = Nothing

$(deriveSafeCopy 1 'base ''AtomicFunction)
