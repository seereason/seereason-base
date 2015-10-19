-- | Define a type describing what sort of ontology things we can
-- comment on.  This might get moved out of happstack-ontology into
-- seereason if we ever decide we we want to comment on non-ontology
-- stuff.
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RankNTypes, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-orphans #-}
module Ontology.Comment
    ( Topic(..)
    , prettyTopic
    ) where

import Control.Applicative ((<$>))
import Data.Data (Data, Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import Ontology.Extra ()
import Ontology.Types  (DocumentId, SubjectId, AssertionId, TheoremId,
                        prettyDocumentId, prettyAssertionId, prettySubjectId, prettyTheoremId, PredicateStyle(AsPredicate))
import Test.QuickCheck (Arbitrary(..), oneof)
import Text.PrettyPrint            (Doc)
import Web.Routes.TH   (derivePathInfo)

data Topic
    = TopicDocument   DocumentId
    | TopicAssertion  AssertionId
    | TopicSubject    SubjectId
    | TopicTheorem    TheoremId
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''Topic)
$(deriveSafeCopy 0 'base ''Topic)

instance Arbitrary Topic where
    arbitrary =
        oneof [ TopicDocument <$> arbitrary
              , TopicAssertion <$> arbitrary
              , TopicSubject <$> arbitrary
              , TopicTheorem <$> arbitrary ]

prettyTopic :: Topic -> Doc
prettyTopic (TopicDocument i)  = prettyDocumentId i
prettyTopic (TopicAssertion i) = prettyAssertionId i
prettyTopic (TopicSubject i)   = prettySubjectId AsPredicate i
prettyTopic (TopicTheorem i)   = prettyTheoremId i
