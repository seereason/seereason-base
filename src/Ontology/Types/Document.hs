{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             OverloadedStrings, TemplateHaskell, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS -Wwarn #-}
module Ontology.Types.Document
    ( Document(Document, documentId', owner, state, text)
    , documentTitle
    , Documents
    ) where

import Data.Char (isSpace)
import Data.Data (Data(..))
import Data.Function (on)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.String
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.IxSet (inferIxSet, noCalcs)
import Happstack.Auth.Core.Profile   (UserId(..))
import Ontology.Types (SubjectId, HasDocumentId(documentId), DocumentId, PrivacyState)

-- |An Document is a piece of information which resides in our database
-- and therefore the fact that this content is associated with a
-- particular Id is not subject to doubt.
data Document
    = Document { documentId' :: DocumentId
               , state :: PrivacyState
               , owner :: UserId
               , text :: T.Text
               }
    -- ^ A text document.  Attributes like author are set using assertions.
    deriving (Data, Typeable)

-- |Build a string that will serve as the document title.  It is the
-- first non-white line of the document trimmed to 70 characters.
documentTitle :: Document -> T.Text
documentTitle x =
    case T.lines (T.dropWhile isSpace (text x)) of
      [] -> fromString "(no title)"
      (t : _) -> takeTitle t
    where
      takeTitle s = T.append (T.take 70 s) (fromString (if T.length s > 70 then "..." else ""))

-- |This allows us to move some of the code involving documents into the Ontology module
instance HasDocumentId Document where
    documentId = documentId'

instance Ord Document where
    compare = compare `on` documentId

instance Eq Document where
    a == b = documentId a == documentId b

$(deriveSafeCopy 1 'base ''Document)
-- $(deriveNewData [''Document])

$(inferIxSet "Documents" ''Document 'noCalcs [''DocumentId, ''SubjectId, ''UserId])

{-
newDocumentP :: OntologyPrimitive formula => SeeReason formula Documents -> (SeeReason formula Documents, Id)
newDocumentP sr = (sr {extradb = (extradb sr) {nextSubject = succ (nextSubject (extradb sr))}}, nextSubject (extradb sr))

-- |Add a new document to the database.
newDocument :: (Monad m, OntologyPrimitive formula) => OntologyT formula Documents extratmp m Id
newDocument = getreason >>= return . newDocumentP >>= \ (sr, a) -> putreason sr >> return a
-}
