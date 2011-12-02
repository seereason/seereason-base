{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module GenI.URL
    ( GenIURL(..)
    ) where

import Data.Generics (Typeable, Data)
import Happstack.Auth.Core.Profile (UserId)
import Ontology.Types (AssertionId, SubjectId)
import Web.Routes.TH (derivePathInfo)

-- | This is similar to Ontology.URL, but we simplify things for the
-- client by not requiring authentication, and we don't have anything
-- that can change the database.
data GenIURL
    = GenI_Assertion AssertionId
    | GenI_Assertions
    | GenI_Subject UserId SubjectId
    | GenI_Subjects
    deriving (Eq, Ord, Typeable, Data)

$(derivePathInfo ''GenIURL)
