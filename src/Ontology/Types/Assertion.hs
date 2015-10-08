{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Ontology.Types.Assertion
    ( -- * AssertionId, Assertion
      Assertion(..)
    , MUser
    , PrivacyState(..)
    , Assertions
    , AssertionId(unAssertionId)
    , findAssertionIds
    , unsafeAssertionId
    , prettyAssertionId
    , successorAssertionIds
    ) where

import Control.Applicative((<$>))
import Data.Data (Data(..))
import Data.Function (on)
import Data.IxSet (IxSet)
import Data.Logic.Classes.Pretty (Pretty(pPrint))
import Data.SafeCopy -- (base, extension, deriveSafeCopy)
import qualified Data.Set.Extra as Set
--import Data.Time (Day(..))
import Data.Time.Clock (UTCTime(..) {-, secondsToDiffTime-})
import Data.Typeable (Typeable)
import Data.UserId (UserId(..))
import Test.QuickCheck (Arbitrary(arbitrary))
import Text.PrettyPrint (Doc, text, (<>))
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
findAssertionIds a = Set.gFind a

unsafeAssertionId :: Integer -> AssertionId
unsafeAssertionId = AssertionId

prettyAssertionId :: AssertionId -> Doc
prettyAssertionId x = text ("A" ++ show (unAssertionId x))

instance Pretty AssertionId where
    pPrint = prettyAssertionId

-- |Find assertions directly referenced by identifier
successorAssertionIds :: Data a => a -> Set.Set AssertionId
successorAssertionIds a = Set.gFind a :: Set.Set AssertionId

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

type MUser = Maybe UserId

-- |The life cycle of an assertion.
data PrivacyState
    = Proposed   -- ^ Under construction, an editing form will be displayed
    | Private    -- ^ Complete, but still visible only to owner, can return to Proposed state
    | Published  -- ^ Public, can not be altered
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance Eq (Assertion formula) where
    a == b = compare a b == EQ

instance Ord (Assertion formula) where
    compare = compare `on` assertionId

instance Pretty formula => Pretty (Assertion formula) where
    pPrint a = pPrint (assertionId a) <> text ":" <> pPrint (proposition a)

type Assertions formula = IxSet (Assertion formula)

$(deriveSafeCopy 1 'base ''Assertion)
$(deriveSafeCopy 1 'base ''AssertionId)
$(deriveSafeCopy 1 'base ''PrivacyState)

{-
instance (New.Data DefaultD f, Default f) => New.Data DefaultD (Assertion f) where
    toConstr = error "toConstr Ontology.Types.Assertion"

instance (New.Data DefaultD f, Default f) => Default (Assertion f) where
    defaultValue = Assertion { created        = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0) 
                             , assertionId    = defaultValue
                             , asserter       = Nothing
                             , proposition    = defaultValue
                             , assertionState = defaultValue
                             }

$(deriveNewDataNoDefault [''AssertionId])
$(deriveNewData [''PrivacyState])

instance Default AssertionId where
    defaultValue = AssertionId 1
-}
