{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, StandaloneDeriving, TemplateHaskell, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}
module Ontology.Types.Formula.AtomicFunction
    ( AtomicFunction(..)
    , prettyAtomicFunction
    ) where

import Data.Data (Data)
import Data.Monoid ((<>))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set as Set (notMember)
import Data.String (IsString(fromString))
import Data.Typeable (Typeable)
import Term (IsFunction, IsVariable)
import Ontology.Arity (HasArity(arity))
import Ontology.Types.Formula.AtomicPredicate (AtomicPredicate(..), prettyAtomicPredicate, prettyNumberLit)
import Ontology.Types (prettySubjectId, PredicateStyle(AsFunction))
import Pretty (Pretty(pPrint))
import Skolem (HasSkolem(..))
import Text.PrettyPrint (Doc, brackets, text)

-- |The atomic function used as a parameter to the
-- 'Logic.Predicate.Formula' type.
data AtomicFunction description v
    = Function (AtomicPredicate description)
    -- ^ If an n-ary predicate is known to match exactly one object,
    -- we can use it to represent an (n-1)-ary function whose "name"
    -- is the n-1-tuple containing the first n-1 arguments to the
    -- predicate and whose value is the matching tuple's nth element.
    -- So a unary predicate like Somebody UserId turns in a constant
    -- function whose value is that user.  Although we cannot
    -- necessarily know that a predicate matches exactly one value, we
    -- shall assume it.  This requires more thought. (FIXME)
    | Skolem v Int -- ^ A temporary value used by the automatic theorem prover
    deriving (Eq, Ord, Data, Typeable, Show)

instance IsString (AtomicFunction description v) where
    fromString = Function . fromString

instance (IsVariable v, Data description, Ord description, Show description, Pretty description) => HasSkolem (AtomicFunction description v) where
    type SVarOf (AtomicFunction description v) = v
    toSkolem = Skolem
    foldSkolem _ sk (Skolem v n) = sk v n
    foldSkolem f _ ap = f ap
    variantSkolem f fns | Set.notMember f fns = f
    variantSkolem (Skolem v n) fns = variantSkolem (Skolem v (succ n)) fns
    variantSkolem _ _ = error $ "Expected skolem function"

instance (Pretty description, Show description, Ord description, Data description, IsVariable v, IsString (AtomicFunction description v)
         ) => IsFunction (AtomicFunction description v)

prettyAtomicFunction :: (Eq description, Ord description, Pretty description, IsVariable v) => AtomicFunction description v -> Doc
prettyAtomicFunction x =
    case x of
      Function (Reference _ ident) -> prettySubjectId AsFunction ident
      Function (NumberLit d) -> prettyNumberLit d
      Function p -> prettyAtomicPredicate AsFunction p
      Skolem v n -> text "Sk" <> (if n == 1 then mempty else (text "." <> text (show n))) <> brackets (pPrint v)

instance (Pretty description, Ord description, IsVariable v) => Pretty (AtomicFunction description v) where
    pPrint = prettyAtomicFunction

instance (Ord description, Pretty description, IsVariable v) => HasArity (AtomicFunction description v) where
    arity (Function p) = maybe Nothing (\ n -> Just (n - 1)) (arity p)
    arity (Skolem _ _) = Nothing

$(deriveSafeCopy 1 'base ''AtomicFunction)
