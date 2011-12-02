{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wwarn #-}
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
    ( -- * AssertionId, Assertion
      Assertion(..)
    , Assertions
    , PrivacyState(Proposed, Private, Published)
    , AssertionId(unAssertionId)
    , findAssertionIds
    , unsafeAssertionId
    , prettyAssertionId
    , successorAssertionIds
    -- * Belief (in an assertion)
    , Belief(Belief, theQuestion, theAnswer, startTime)
    , Answer(Answer, Nonsense)
    -- * SubjectId
    , PredicateStyle(..)
    , SubjectId(unSubjectId)
    , unsafeSubjectId
    , prettySubjectId
    -- * SubjectNode
    , SubjectNode(..)
    , prettySubjectNode
    , SubjectEdge
    , prettyEdge
    , SubjectEdges
    -- * Subject
    , MUser
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
    -- * PredForm
    , PredForm
    , foldPred
    , makePred
    -- * FunctionId
    , FunctionId(unFunctionId)
    , prettyFunctionId
    , unsafeFunctionId
    -- * DocumentId
    , HasDocumentId(documentId)
    , DocumentId(unDocumentId)
    , findDocumentIds
    , unsafeDocumentId
    , prettyDocumentId
    -- * TheoremId, Theorem
    , Theorem(Theorem, theoremOwner, theoremId, argument, theoremPrivacy)
    , TheoremId(unTheoremId)
    , findTheoremIds
    , unsafeTheoremId
    , prettyTheoremId
    , Theorems
    , DB_Error(..)
    ) where

import Control.Applicative((<$>))
import Data.Data (Data(..))
import Data.Function (on)
import qualified Data.Generics.SYB.WithClass.Basics as New
import Data.Logic (FirstOrderFormula(foldFirstOrder), Term(var), Variable(one, next), pApp, Predicate(Apply), Arity(arity), Negatable(..), ProofResult(..))
import qualified Data.Map as Map
import Data.SafeCopy -- (base, extension, deriveSafeCopy)
import qualified Data.Set.Extra as Set
import Data.Time       (Day(..))
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Typeable (Typeable)
import Happstack.Data (Default(defaultValue), DefaultD, gFind, deriveNewData, deriveNewDataNoDefault, gFind)
import Data.IxSet (IxSet, inferIxSet, noCalcs)
import Happstack.Auth.Core.Profile   (UserId(..))
import Test.QuickCheck (Arbitrary(arbitrary), oneof)
import Text.JSON (JSON(readJSON, showJSON), makeObj, valFromObj, JSValue(JSObject))
import Text.PrettyPrint (Doc, text, cat, brackets, sep)
import Web.Routes.TH (derivePathInfo)

-- |The advantage of making this a newtype is that we can build an
-- IxSet index for this type only, and a different one for SubjectId.
newtype AssertionId = AssertionId {unAssertionId :: Integer} deriving (Data, Typeable, Eq, Ord, Read)

instance Arbitrary AssertionId where
    arbitrary = (AssertionId <$> arbitrary)

instance Show AssertionId where
    show x = "(unsafeAssertionId " ++ show (unAssertionId x) ++ ")"

$(derivePathInfo ''AssertionId)

findAssertionIds :: Data a => a -> Set.Set AssertionId
findAssertionIds a = Set.fromList (gFind a :: [AssertionId])

unsafeAssertionId :: Integer -> AssertionId
unsafeAssertionId = AssertionId

prettyAssertionId :: AssertionId -> Doc
prettyAssertionId x = text ("A" ++ show (unAssertionId x))

-- |Find assertions directly referenced by identifier
successorAssertionIds :: Data a => a -> Set.Set AssertionId
successorAssertionIds a = Set.gFind a :: Set.Set AssertionId

-- |A belief is the association of an assertion with a truth value
-- indicating whether the assertion is accepted, rejected, or that one
-- has no opinion.  The Believers predicate above is used to construct
-- a set of people who accept or reject an assertion.  The user is
-- considered to have this belief about the assertion starting from
-- the time the belief is created until the creation of a new belief about
-- the same assertion.  This is why the truth value is a maybe, so that
-- a user who accepted or rejected an assertion to later have no opinion.
data Belief
    = Belief { theQuestion :: AssertionId -- You believe this assertion is...
             , theAnswer :: Answer        -- always true, always false, or could be either...
             , startTime :: UTCTime       -- as of this time.
             } deriving (Data, Typeable, Show)

data Answer
    = Answer ProofResult
    | Nonsense
    deriving (Eq, Ord, Data, Typeable, Show)

instance Ord Belief where
    compare a b =
        foldl (\ ordering cmp -> if ordering == EQ then cmp a b else ordering) EQ
                  [compare `on` theQuestion,
                   flip (compare `on` startTime),
                   compare `on` theAnswer]

instance Eq Belief where
    a == b = compare a b == EQ

-- |An Assertion is a Proposition which is associated with a user.
-- Thus, the user is considered to have asserted the proposition.
-- However, making an assertion is not the same as accepting or
-- rejecting that assertion, that is done by asserting a Belief about
-- the assertion.
data Assertion formula
    = Assertion { created :: UTCTime
                , assertionId :: AssertionId
                , asserter :: MUser
                , proposition :: formula
                , assertionState :: PrivacyState }
    -- ^ An Assertion is a proposition which is asserted by a user, with
    -- some revision information including an identifying number, a timestamp,
    -- and a revision number.  The owner can create new revisions of the
    -- assertion, but references to assertions are to a particular
    -- revision, not to just the assertion identifier.
    | Tautology { proposition :: formula }
    -- ^ An assertion which is not subject to acceptance or rejection.
    -- For example, when we create an Object which assigns a Id
    -- to a Document, the proposition which asserts that that Id
    -- is that document is a tautology.
    deriving (Data, Typeable, Show)

instance Eq (Assertion formula) where
    a == b = compare a b == EQ

instance Ord (Assertion formula) where
    compare = compare `on` assertionId

type Assertions formula = IxSet (Assertion formula)

-- |The life cycle of an assertion.
data PrivacyState
    = Proposed   -- ^ Under construction, an editing form will be displayed
    | Private    -- ^ Complete, but still visible only to owner, can return to Proposed state
    | Published  -- ^ Public, can not be altered
    deriving (Read, Show, Eq, Ord, Data, Typeable)

newtype SubjectId = SubjectId {unSubjectId :: Integer} deriving (Data, Typeable, Eq, Ord, Read)

instance Show SubjectId where
    show x = "(unsafeSubjectId " ++ show (unSubjectId x) ++ ")"

-- |Predicates can be used and rendered as functions, this controls whether to do so.
data PredicateStyle = AsPredicate | AsFunction deriving Show

prettySubjectId :: PredicateStyle -> SubjectId -> Doc
prettySubjectId AsPredicate x = text ("S" ++ show (unSubjectId x))
prettySubjectId AsFunction x = text ("F" ++ show (unSubjectId x))

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
    } deriving (Data, Typeable)

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

-- |A PredForm is a wrapper around a formula that indicates that
-- the formula is of the form Apply p [term].
newtype PredForm formula = PredForm formula
    deriving (Eq, Ord, Data, Typeable, Read, Show)

-- ^ This function is used to access the predicate in a PredForm.
-- Note that the type "a" might be a function such as "[term] -> b",
-- which means you can simulate the pApp function.
foldPred :: FirstOrderFormula formula term v p f => (p -> a) -> PredForm formula -> a
foldPred fn (PredForm form) =
    foldFirstOrder q c p form
    where
      -- 
      p (Apply ap _) = fn ap
      -- We don't have to implement anything else, because we know
      -- this first case will match.
      p _ = undefined
      q = undefined
      c = undefined

-- |Create a PredForm from an atomic predicate and some generated terms.
makePred :: FirstOrderFormula formula term v p f => p -> PredForm formula
makePred p = PredForm (pApp p ts)
    where ts = case arity p of
                 Nothing -> error "makePred: Fixed arity expected"
                 Just n -> take n (map var (iterate next one))

instance FirstOrderFormula formula term v p f => Arity (PredForm formula) where
    arity = foldPred arity

data SubjectNode = Normal {unSubjectNode :: SubjectId} | Complement {unSubjectNode :: SubjectId} deriving (Eq, Data, Typeable)
type SubjectEdge formula = (SubjectNode, SubjectNode, Assertion formula)
type SubjectEdges formula = Set.Set (SubjectEdge formula)

prettySubjectNode :: PredicateStyle -> SubjectNode -> Doc
prettySubjectNode style (Complement s) = cat [text "~", prettySubjectId style s]
prettySubjectNode style (Normal s) = prettySubjectId style s

prettyEdge :: PredicateStyle -> SubjectEdge formula -> Doc
prettyEdge style (s1, s2, _) = cat [prettySubjectNode style s1, text "->", prettySubjectNode style s2]

instance Negatable SubjectNode where
    negated (Normal _) = False
    negated _ = True
    (.~.) (Complement x) = Normal x
    (.~.) (Normal x) = Complement x

-- |We want Normal nodes to come before Complement nodes, because normally
-- a Subject will have at least one Normal node so we can expect minId to
-- return it.
instance Ord SubjectNode where
    compare (Normal _) (Complement _) = LT
    compare (Complement _) (Normal _) = GT
    compare s1 s2 = compare (unSubjectNode s1) (unSubjectNode s2)

type MUser = Maybe UserId

-- |We may need to add more information to this.  A Subject is an
-- equivalence class of Id, as defined by the current assertion
-- set and the current user.  The list of SubjectTuple contains
-- information about the set of defining assertions which were
-- computed for those Ids.
data Subject formula
    = Subject
      { subjectEquivalenceSet :: Set.Set (Assertion formula)
      -- ^ The assertions which formed the equivalance set,
      -- i.e. the edges of the SCC.
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
      } deriving (Data, Typeable)

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

instance FirstOrderFormula formula term v p f => Arity (SubjectTuple formula) where
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

-- |A type to identify functions defined using the ontology mechanism
-- the same way subjects are.
newtype FunctionId = FunctionId {unFunctionId :: Integer} deriving (Data, Typeable, Eq, Ord, Read)

instance Show FunctionId where
    show x = "(unsafeFunctionId " ++ show (unFunctionId x) ++ ")"

prettyFunctionId :: FunctionId -> Doc
prettyFunctionId x = text ("F" ++ show (unFunctionId x))

unsafeFunctionId :: Integer -> FunctionId
unsafeFunctionId = FunctionId

instance JSON FunctionId where
    showJSON sid = makeObj [ ("functionId", showJSON (unFunctionId sid)) ]
    readJSON (JSObject jsobj) = unsafeFunctionId <$> valFromObj "functionId" jsobj
    readJSON _ = error "Unexpected JSON for FunctionId"

-- |Each value of type Subject represents a set.  The elements of the
-- set are determined by the propositions in which the subject appears
-- via the Reference predicate.  Thus, by itself a Subject is just a
-- meaningless identifier which can be referred to in propositions.
-- It refers to an implied predicate which consists of the context
-- formed by the propositions in which it appears.  Users can agree or
-- disagree with each of these propositions (by adding assertions
-- containing Believer predicates about themselves.)
data DocumentId = DocumentId {unDocumentId :: Integer} deriving (Read, Eq, Ord, Data, Typeable)

prettyDocumentId :: DocumentId -> Doc
prettyDocumentId x = text ("D" ++ show (unDocumentId x))

instance Show DocumentId where
    show x = "(unsafeDocumentId " ++ show (unDocumentId x) ++ ")"

-- |Return a set of all the Ids that occur in the value.  It
-- would be good to have a Set version of gFind.  Frequently the
-- result gets immediately turned back into a list, but we still want
-- to avoid duplicates.
findDocumentIds :: Data a => a -> Set.Set DocumentId
findDocumentIds a = Set.fromList (gFind a :: [DocumentId])

instance Arbitrary DocumentId where
    arbitrary = (DocumentId <$> arbitrary)

-- |This is a quick hack to keep the Id constructor private.
-- readJSONId :: JSObject JSValue -> Result Id
-- readJSONId jsval = valFromObj "val" jsval >>= return . Id

unsafeDocumentId :: Integer -> DocumentId
unsafeDocumentId = DocumentId

-- |The document type is given as a parameter to the LogicT monad, but
-- it must be an instance of this so we know how to get an id from 
-- the document to query the document IxSet.
class HasDocumentId a where
    documentId :: a -> DocumentId

$(derivePathInfo ''DocumentId)

-- |A Theorem is a list of assertions.  These are passed to
-- the theorem prover.
data Theorem =
    Theorem { theoremOwner :: UserId
            , theoremId :: TheoremId
            , argument :: [AssertionId]
            , theoremPrivacy :: PrivacyState
            } deriving (Data, Typeable)

-- |Only one theorem including a given list of assertions can be
-- entered into the system.  (Should this be a set?  Or a set and
-- a conclusion assertion?)
instance Ord Theorem where
    compare = compare `on` argument

instance Eq Theorem where
    a == b = compare a b == EQ

data TheoremId = TheoremId {unTheoremId :: Integer} deriving (Read, Eq, Ord, Data, Typeable)

instance Show TheoremId where
    show x = "(unsafeTheoremId " ++ show (unTheoremId x) ++ ")"

$(derivePathInfo ''TheoremId)

prettyTheoremId :: TheoremId -> Doc
prettyTheoremId x = text ("T" ++ show (unTheoremId x))

unsafeTheoremId :: Integer -> TheoremId
unsafeTheoremId = TheoremId

findTheoremIds :: Data a => a -> Set.Set TheoremId
findTheoremIds a = Set.fromList (gFind a :: [TheoremId])

instance Arbitrary TheoremId where
    arbitrary = (TheoremId <$> arbitrary)

$(deriveSafeCopy 1 'base ''Assertion)
$(deriveSafeCopy 1 'base ''AssertionId)
$(deriveSafeCopy 1 'base ''SubjectId)
$(deriveSafeCopy 1 'base ''SubjectNode)
$(deriveSafeCopy 1 'base ''PrivacyState)
$(deriveSafeCopy 1 'base ''PredForm)
$(deriveSafeCopy 1 'base ''DocumentId)
$(deriveSafeCopy 2 'extension ''Theorem)
$(deriveSafeCopy 1 'base ''TheoremId)
$(deriveSafeCopy 2 'extension ''Belief)
$(deriveSafeCopy 1 'base ''Answer)

instance (New.Data DefaultD f, Default f) => New.Data DefaultD (Assertion f) where
    toConstr = error "toConstr Ontology.Types.Assertion"

instance (New.Data DefaultD f, Default f) => Default (Assertion f) where
    defaultValue = Assertion { created        = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0) 
                             , assertionId    = defaultValue
                             , asserter       = Nothing
                             , proposition    = defaultValue
                             , assertionState = defaultValue
                             }

instance (New.Data DefaultD Theorem) where 
    toConstr = error "toConstr Ontology.Types.Theorem"

instance Default Theorem where
    defaultValue = Theorem { theoremOwner = UserId 0
                           , theoremId    = defaultValue
                           , argument     = defaultValue
                           , theoremPrivacy = Proposed
                           }

-- It would be better not to have a Default instance for the Id types
$(deriveNewDataNoDefault [''AssertionId, ''SubjectId, ''SubjectNode, ''DocumentId, ''TheoremId, ''Belief, ''Answer])
$(deriveNewData [''PrivacyState, ''PredForm])

instance Default AssertionId where
    defaultValue = AssertionId 1

instance Default SubjectId where
    defaultValue = SubjectId 1

instance Default SubjectNode where
    defaultValue = Normal defaultValue

instance Default TheoremId where
    defaultValue = TheoremId 1

instance Default DocumentId where
    defaultValue = DocumentId 1

instance Default Answer where
    defaultValue = Answer defaultValue

$(inferIxSet "Theorems" ''Theorem 'noCalcs [''TheoremId, ''AssertionId])

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

$(deriveSafeCopy 1 'base ''DB_Error)

-- Migration

data Belief_v1
    = Belief_v1
      { theQuestion_v1 :: AssertionId
      , theAnswer_v1 :: Maybe Bool
      , startTime_v1 :: UTCTime
      } deriving (Data, Typeable, Show)

$(deriveSafeCopy 1 'base ''Belief_v1)

instance Migrate Belief where
    type MigrateFrom Belief = Belief_v1
    migrate x@(Belief_v1 {}) = Belief {theQuestion = theQuestion_v1 x,
                                       theAnswer = maybe (Answer Invalid) (\ flag -> Answer (if flag then Proved else Disproved)) (theAnswer_v1 x),
                                       startTime = startTime_v1 x}

data Theorem_v1 =
    Theorem_v1 { theoremOwner_v1 :: UserId
               , theoremId_v1 :: TheoremId
               , argument_v1 :: [AssertionId]
               } deriving (Data, Typeable)

$(deriveSafeCopy 1 'base ''Theorem_v1)

instance Migrate Theorem where
    type MigrateFrom Theorem = Theorem_v1
    migrate x@(Theorem_v1 {}) = Theorem { theoremOwner = theoremOwner_v1 x,
                                          theoremId = theoremId_v1 x,
                                          argument = argument_v1 x,
                                          theoremPrivacy = Proposed }
