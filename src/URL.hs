{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans -fspec-constr-count=3 #-}
module URL where

import Control.Applicative((<$>))
import Data.Data(Data)
import Data.Typeable(Typeable)
import GenI.URL (GenIURL(..))
import Happstack.Auth.Core.AuthURL    (AuthURL(..))
import Happstack.Auth.Core.ProfileURL (ProfileURL(..))
import Ontology.Comment (Topic(..))
import qualified Ontology.URL as Ontology
import qualified Preferences.URL as Preferences
import qualified Scaffolding.Comment.URL as Comment
import qualified Scaffolding.ProfileData.URL as ProfileData
import Test.QuickCheck (Arbitrary(arbitrary), oneof {-, Property, property-})
import Web.Routes.TH (derivePathInfo)

data WebURL 
    = W_Homepage
    | W_Auth AuthURL
    | W_Profile ProfileURL
    | W_ProfileData ProfileData.URL
    | W_Comment (Comment.URL Topic)
    | W_Ontology Ontology.URL
    | W_GenI GenIURL
    | W_Dump
    -- | W_Reload -- too dangerous to compile into production server
    | W_UnicodeKey
    | W_Help
      deriving (Eq, Ord, Typeable, Data)

$(derivePathInfo ''WebURL)

instance Arbitrary WebURL where
    arbitrary = oneof [ return $ W_Homepage
                      , W_Ontology <$> arbitrary
                      -- , return W_Reload
                      ]

instance ProfileData.MkURL WebURL where
    mkURL = W_ProfileData
    authURL = W_Auth
    userURL = W_Ontology . Ontology.Account
    profileURL = W_Profile

{-
-- to use run, quickCheck url_prop
url_prop :: Property
url_prop = property (pathInfoInverse_prop :: WebURL -> Bool)
-}

-- | How to build a WebURL from a Comment URL or a Topic.
instance Comment.MkURL Topic WebURL where
    mkURL = W_Comment
    topicURL (TopicDocument did) = W_Ontology . Ontology.Document $ did
    topicURL (TopicAssertion aid) = W_Ontology . Ontology.Assertion $ aid
    topicURL (TopicSubject sid) = W_Ontology . Ontology.Subject $ sid
    topicURL (TopicTheorem tid) = W_Ontology . Ontology.Theorem $ tid

instance Ontology.MkURL WebURL where
  mkURL = W_Ontology
  mkAssertionIdURL = Ontology.mkURL . Ontology.Assertion
  mkSubjectIdURL = Ontology.mkURL . Ontology.Subject
  mkDocumentIdURL = Ontology.mkURL . Ontology.Document
  mkUserIdURL = Ontology.mkURL . Ontology.Account

instance Preferences.MkURL WebURL where
  mkURL = W_Ontology . Ontology.Preferences
