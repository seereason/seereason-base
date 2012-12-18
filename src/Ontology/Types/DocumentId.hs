{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Ontology.Types.DocumentId
    ( HasDocumentId(documentId)
    , DocumentId(unDocumentId)
    , findDocumentIds
    , unsafeDocumentId
    , prettyDocumentId
    ) where

import Control.Applicative((<$>))
import Data.Data (Data(..))
import Data.Logic.Classes.Pretty (Pretty(pretty))
import Data.SafeCopy -- (base, extension, deriveSafeCopy)
import qualified Data.Set.Extra as Set
import Data.Typeable (Typeable)
import Data.Generics.Extra (gFind)
import Test.QuickCheck (Arbitrary(arbitrary))
import Text.PrettyPrint (Doc, text)
import Web.Routes.TH (derivePathInfo)

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

instance Pretty DocumentId where
    pretty = prettyDocumentId

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
$(deriveSafeCopy 1 'base ''DocumentId)
