{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Ontology.Types.Subject
    (
    -- * SubjectId
      PredicateStyle(..)
    , SubjectId(unSubjectId)
    , unsafeSubjectId
    , prettySubjectId
    -- * SubjectNode
    , SubjectNode(..)
    , subjectIds'
    , prettySubjectNode
    , SubjectEdge
    , prettyEdge
    , SubjectEdges
    -- * Subject
    , Subject(subjectEquivalenceSet, subjectIds, subjectDefinitionMap, subjectArity)
    , prettySubject
    , unsafeSubject
    , minId
    , minId'
    , subjectAssertions
    , subjectDefinitionIds
    , subjectTuples
    , SubjectTuple (theAssertion, theSubject, thePredicate, theBelief)
    , unsafeSubjectTuple
    ) where

import Control.Applicative((<$>))
import Data.Data (Data(..))
import Data.Function (on)
import Data.Logic.Classes.Arity (Arity(arity))
import Data.Logic.Classes.Equals (AtomEq)
import Data.Logic.Classes.Pretty (Pretty(pPrint))
import Data.Logic.Classes.Negate (Negatable(..))
import Data.Logic.Classes.FirstOrder (FirstOrderFormula)
import Data.Logic.KnowledgeBase (ProofResult(..))
import qualified Data.Map as Map
import Data.SafeCopy -- (base, extension, deriveSafeCopy)
import qualified Data.Set.Extra as Set
import Data.Typeable (Typeable)
import Ontology.Types.Assertion (Assertion(assertionId), AssertionId)
import Ontology.Types.PredForm (PredForm, foldPred)
import Test.QuickCheck (Arbitrary(arbitrary), oneof)
import Text.JSON (JSON(readJSON, showJSON), makeObj, valFromObj, JSValue(JSObject))
import Text.PrettyPrint (Doc, text, cat, brackets, sep)
import Web.Routes.TH (derivePathInfo)

newtype SubjectId = SubjectId {unSubjectId :: Integer} deriving (Data, Typeable, Eq, Ord, Read)

instance Show SubjectId where
    show x = "(unsafeSubjectId " ++ show (unSubjectId x) ++ ")"

-- |Predicates can be used and rendered as functions, this controls whether to do so.
data PredicateStyle = AsPredicate | AsFunction deriving Show

prettySubjectId :: PredicateStyle -> SubjectId -> Doc
prettySubjectId AsPredicate x = text ("S" ++ show (unSubjectId x))
prettySubjectId AsFunction x = text ("F" ++ show (unSubjectId x))

instance Pretty SubjectId where
    pPrint = prettySubjectId AsPredicate

instance JSON SubjectId where
    showJSON sid = makeObj [ ("subjectId", showJSON (unSubjectId sid)) ]
    readJSON (JSObject jsobj) = unsafeSubjectId <$> valFromObj "subjectId" jsobj
    readJSON _ = error "Unexpected JSON for SubjectId"

-- |A tuple which represents one of a subject's definitions, plus a
-- flag containing the current user's opinion about the the
-- definition, either accepts, rejects, or nothing.  These tuples are
-- computed based on an assertions in the database of this form:
-- 
--      @S99(x, y, ...) => p(x, y, ...)@
-- 
-- where S99 is a reference to a SubjectId and p is any predicate, and
-- (x, y, ...) are all of the free variables in the formula.  The
-- foldAssertion function in Ontology.HasOntology is used to recognize this
-- form.

data SubjectTuple formula =
    SubjectTuple
    { theAssertion :: Assertion formula      -- ^ The assertion which combines the subject and predicate below
    , theSubject :: SubjectId                -- ^ The subject being defined
    , thePredicate :: PredForm formula       -- ^ The predicate which describes what the subject includes
    , theBelief :: ProofResult               -- ^ Whether the user explicitly accepts or rejects the assertion, or neither
    } deriving (Data, Typeable, Show)

-- |This Ord instance gives us the most interesting predicates first,
-- rather than sorting by subject number.
instance Ord formula => Ord (SubjectTuple formula) where
    compare a b =
        foldl (\ ordering cmp -> if ordering == EQ then cmp a b else ordering) EQ
              [compare `on` thePredicate,
               compare `on` (beliefOrder . theBelief),
               compare `on` theSubject]
        where beliefOrder Proved = (0 :: Int)
              beliefOrder Invalid = (1 :: Int)
              beliefOrder Disproved = (2 :: Int)

instance Ord formula => Eq (SubjectTuple formula) where
    a == b = compare a b == EQ

unsafeSubjectTuple :: Assertion formula -> SubjectId -> PredForm formula -> ProofResult -> SubjectTuple formula
unsafeSubjectTuple a s p b = SubjectTuple {theAssertion = a, theSubject = s, thePredicate = p, theBelief = b}

unsafeSubjectId :: Integer -> SubjectId
unsafeSubjectId = SubjectId

data SubjectNode = Normal {unSubjectNode :: SubjectId} | Complement {unSubjectNode :: SubjectId} deriving (Eq, Data, Typeable, Show)
type SubjectEdge formula = (SubjectNode, SubjectNode, Assertion formula)
type SubjectEdges formula = Set.Set (SubjectEdge formula)

prettySubjectNode :: PredicateStyle -> SubjectNode -> Doc
prettySubjectNode style (Complement s) = cat [text "~", prettySubjectId style s]
prettySubjectNode style (Normal s) = prettySubjectId style s

prettyEdge :: PredicateStyle -> SubjectEdge formula -> Doc
prettyEdge style (s1, s2, _) = cat [prettySubjectNode style s1, text "->", prettySubjectNode style s2]

instance Negatable SubjectNode where
    negatePrivate (Complement x) = Normal x
    negatePrivate (Normal x) = Complement x
    foldNegation _ inverted (Complement x) = inverted (Normal x)
    foldNegation normal _ (Normal x) = normal (Normal x)

-- |We want Normal nodes to come before Complement nodes, because normally
-- a Subject will have at least one Normal node so we can expect minId to
-- return it.
instance Ord SubjectNode where
    compare (Normal _) (Complement _) = LT
    compare (Complement _) (Normal _) = GT
    compare s1 s2 = compare (unSubjectNode s1) (unSubjectNode s2)

-- |We may need to add more information to this.  A Subject is an
-- equivalence class of Id, as defined by the current assertion
-- set and the current user.  The list of SubjectTuple contains
-- information about the set of defining assertions which were
-- computed for those Ids.
data Subject formula
    = Subject
      { subjectEquivalenceSet :: Set.Set (Assertion formula)
      -- ^ The assertions which formed the equivalance set, i.e. the
      -- edges of the SCC.  A better name would be subjectNodes.
      , subjectIds :: Set.Set SubjectNode
      -- ^ The collection of Ids which comprise the subject,
      -- i.e. the nodes of the SCC.  Note that the keys are SubjectId rather
      -- than SubjectNode, this is because it is easy to complement
      -- the SubjectTuples associated with the SubjectId, we just
      -- complement the tuple's assertion.
      , subjectDefinitionMap :: Map.Map SubjectId (Set.Set (SubjectTuple formula))
      -- ^ The defining assertions for the subject.  The
      -- keys are the nodes of the SCC.
      , subjectArity :: Maybe Int
      -- ^ If the subject has any definitions, the arity of all
      -- definitions must match, and this will hold that value.
      } deriving (Data, Typeable, Show)

subjectIds' :: Subject formula -> Set.Set SubjectId
subjectIds' subj = Set.map unSubjectNode (subjectIds subj)

unsafeSubject :: Set.Set (Assertion formula)
              -> Set.Set SubjectNode
              -> Map.Map SubjectId (Set.Set (SubjectTuple formula))
              -> Maybe Int
              -> Subject formula
unsafeSubject = Subject 

prettySubject :: PredicateStyle -> Subject formula -> Doc
prettySubject style = brackets . sep . map (prettySubjectNode style) . Set.toList . subjectIds

instance Ord (Subject formula) where
    compare = compare `on` minId

instance Eq (Subject formula) where
    a == b = subjectIds a == subjectIds b

-- | The mimimal element of the Id set uniquely identifies the Subject
-- with respect to a given user and assertion set.  A subject created
-- from a SubjectId will always have a Normal SubjectNode as its
-- minimum element.  The minId' function below returns the minimum
-- subjectNode, for use with subjects created by the subject'
-- function.
minId :: Subject formula -> SubjectId
minId s = case minId' s of
            Normal i -> i
            Complement i -> i -- error $ "minId: subject " ++ show (prettySubject s) ++ " contains no uncomplemented nodes."

minId' :: Subject formula -> SubjectNode
minId' = Set.findMin . subjectIds

subjectAssertions :: Ord formula => Subject formula -> Set.Set (Assertion formula)
subjectAssertions = Set.map theAssertion . subjectTuples

-- |Flatten the subject definition sets.
subjectDefinitionIds :: Ord formula => Subject formula -> Set.Set AssertionId
subjectDefinitionIds = Set.map assertionId . subjectAssertions

-- |Get a subject's list of assertions and return the merged list of
-- subject tuples.  (This is crazy, we computed all this when we built
-- the subject.)
subjectTuples :: Ord formula => Subject formula -> Set.Set (SubjectTuple formula)
subjectTuples = Set.unions . Map.elems . subjectDefinitionMap

instance (FirstOrderFormula formula atom v, AtomEq atom p term) => Arity (SubjectTuple formula) where
    arity = foldPred arity . thePredicate

{-
instance FirstOrderFormula formula term v p f => Arity (Subject formula) where
    arity = pairsArity . Map.toList . subjectDefinitionMap
        where
          pairsArity [] = Nothing
          pairsArity (pr : prs) = maybe (pairsArity prs) Just (pairArity pr)
          pairArity (_, ts) = tuplesArity (Set.toList ts)
          tuplesArity [] = Nothing
          tuplesArity (t : ts) = maybe (tuplesArity ts) Just (arity t)
-}

instance Arbitrary SubjectId where
    arbitrary = (SubjectId <$> arbitrary)

$(derivePathInfo ''SubjectId)

instance Arbitrary SubjectNode where
    arbitrary = oneof [ Normal <$> arbitrary, Complement <$> arbitrary ]
$(deriveSafeCopy 1 'base ''SubjectId)
$(deriveSafeCopy 1 'base ''SubjectNode)
