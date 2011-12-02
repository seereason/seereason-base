{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             StandaloneDeriving, TemplateHaskell, UndecidableInstances #-}
{-# OPTIONS -Wall -Werror #-}
module Ontology.Types.Description
    ( LinguisticHint(..)
    , SubDocument(..)
    , getSubDocument
    , TextRange(..)
    , NounPhraseFragment(..)
    , textOfNounPhraseFragment
    , Description
    , np, ap, asp
    ) where

import Data.Data (Data(..))
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Happstack.Data (Default(defaultValue), deriveNewData, deriveNewDataNoDefault)
import Data.IxSet (toList, (@=))
import Ontology.Types (DocumentId(..), prettyDocumentId, SubjectId(..))
import Ontology.Types.Document (Document(text), Documents)

-- |Hints used to combine descriptions, they should be presented in
-- the order given here.
data LinguisticHint
    = AdjectivePhrase       -- ^ e.g. "very wealthy."
    | NounPhrase            -- ^ e.g. "the wealthy."
    | AdjectiveSuffixPhrase -- ^ e.g. "of great wealth."
    deriving (Read, Show, Eq, Ord, Data, Typeable)

np :: LinguisticHint
np = NounPhrase
ap :: LinguisticHint
ap = AdjectivePhrase
asp :: LinguisticHint
asp = AdjectiveSuffixPhrase

data SubDocument
    = TextRanges {textRanges :: [TextRange], docId :: DocumentId}
    | Quotation {quotation :: T.Text, docId :: DocumentId}
    deriving (Eq, Ord, Data, Typeable, Show)

data TextRange
    = TextRange {startPos :: Int, endPos :: Int}
    deriving (Eq, Ord, Data, Typeable, Show)

-- |Assuming the second argument is the Document referred to by the
-- subject number in the SubDocument argument, return a Document
-- containing the referred to text.
getSubDocument :: SubDocument -> Document -> Maybe Document
getSubDocument (TextRanges {textRanges = ranges}) doc =
    Just (doc { text = T.intercalate (T.pack " ") (map get ranges) })
    where
      get :: TextRange -> T.Text
      get (TextRange s e) = T.take (e - s + 1) (T.drop s (text doc))
getSubDocument (Quotation t _) d =
    Just (d {text = t})

-- | A free form text description of a set.
data NounPhraseFragment =
    T T.Text | S SubjectId | D SubDocument | Number Double | Percentage Double
    deriving (Data, Typeable, Show)

instance Ord NounPhraseFragment where
    compare (T a) (T b) =
        -- Get shorter strings to sort before longer.
        case compare (T.length a) (T.length b) of
          EQ -> compare a b
          x -> x
    compare (S a) (S b) = compare a b
    compare (D a) (D b) = compare a b
    compare (Number a) (Number b) = compare a b
    compare (Percentage a) (Percentage b) = compare a b
    compare (T _) _ = LT
    compare _ (T _) = GT
    compare (S _) _ = LT
    compare _ (S _) = GT
    compare (D _) _ = LT
    compare _ (D _) = GT
    compare (Number _) _ = LT
    compare _ (Number _) = GT

instance Eq NounPhraseFragment where
    a == b = compare a b == EQ
            
textOfNounPhraseFragment :: Documents -> NounPhraseFragment -> [T.Text]
textOfNounPhraseFragment _documents (T t) = T.words t
textOfNounPhraseFragment _documents (S _)   = [] -- dereference Subject
textOfNounPhraseFragment _documents (Number d)  = [T.pack $ show d]
textOfNounPhraseFragment _documents (Percentage d) = [T.pack $ show d ++ "%"]
textOfNounPhraseFragment documents (D sd) = lookupSD sd
    where
      lookupSD :: SubDocument -> [T.Text]
      lookupSD (Quotation q _aid) = T.words q
{-
          case lookupAss aid of 
            Nothing -> error $ "searchIndex: invalid index: " ++ show aid
            (Just (Document _ (Text _authors title _text)) -> [quot, title]
-}
      lookupSD (TextRanges ranges aid) =
          case lookupAss aid of 
            Nothing -> [] -- error $ "searchIndex: invalid index: " ++ show r
            Just doc -> map (lookupRange (text doc)) ranges

      lookupRange :: T.Text -> TextRange -> T.Text
      lookupRange body (TextRange start end) = 
          T.take (max 0 ((end - start) + 1)) $ T.drop start body 
      
      lookupAss :: DocumentId -> Maybe Document
      lookupAss aid =
          case toList (documents @= aid) of
            [x] -> Just x
            []  -> Nothing
            _ -> error $ "searchIndex: multiple documents for " ++ show (prettyDocumentId aid)

{-
type NewDescription =
    { phrase :: [NounPhraseFragment]
    , related :: [NounPhraseFragment] }
-}

type Description = (LinguisticHint, [NounPhraseFragment])

$(deriveSafeCopy 1 'base ''SubDocument)
$(deriveSafeCopy 1 'base ''TextRange)
$(deriveSafeCopy 1 'base ''LinguisticHint)
$(deriveSafeCopy 1 'base ''NounPhraseFragment)

$(deriveNewData [''LinguisticHint, ''SubDocument, ''TextRange])
$(deriveNewDataNoDefault [''NounPhraseFragment])

instance Default NounPhraseFragment where
    defaultValue = T (T.empty)
