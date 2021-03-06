{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS -Wall -Wwarn #-}
module Ontology.URL
    ( URL(..)
    , TheoremEditorURL(..)
    , MkURL(..)
    ) where

import Data.Data
import Data.UserId (UserId)
import Ontology.Extra ()
import Ontology.Types (AssertionId, DocumentId, SubjectId, TheoremId)
import qualified Preferences.URL as Preferences
import Test.QuickCheck (Arbitrary(..),oneof)
import Web.Routes.TH (derivePathInfo)

data TheoremEditorURL 
    = TheoremEditor
    | TheoremEditorJS
    | RelatedAssertions
    | AssertionInfo
    | SubmitTheorem
      deriving (Eq, Ord, Read, Show, Data, Typeable)

data URL
    = Account UserId
    | Users
    | Assertion AssertionId
    | Assertions
    | NewAssertion
    | Document DocumentId
    | Documents
    | UserDocuments UserId
    | NewDocument
    | Subject SubjectId
    | Subjects
    | Theorem TheoremId
    | TheoremEdit TheoremEditorURL
    | Theorems
    | SubjectAndDocument DocumentId SubjectId
    | AssociateSubjectAndDocument DocumentId SubjectId
    | Search
    | SearchSubjects DocumentId
    | SearchSubjectsAutocomplete DocumentId
    | Preferences Preferences.URL
    deriving (Eq, Ord, Typeable, Data)

class MkURL url where
    mkURL :: URL -> url
    mkAssertionIdURL :: AssertionId -> url
    mkSubjectIdURL :: SubjectId -> url
    mkDocumentIdURL :: DocumentId -> url
    mkUserIdURL :: UserId -> url

instance MkURL URL where
    mkURL = id
    mkAssertionIdURL = Assertion
    mkSubjectIdURL = Subject
    mkDocumentIdURL = Document
    mkUserIdURL = Account

$(derivePathInfo ''TheoremEditorURL)
$(derivePathInfo ''URL)

instance Arbitrary URL where
    arbitrary = oneof [ Document <$> arbitrary ]
