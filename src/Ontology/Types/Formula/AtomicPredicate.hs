{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS -Wall -Wwarn #-}
module Ontology.Types.Formula.AtomicPredicate
    ( AtomicPredicate(..)
    , prettyAtomicPredicate
    , prettyUserId
    , prettyNumberLit
    -- , specificity
    ) where

import Data.Data (Data)
import Data.List (isSuffixOf)
import Data.Logic.Classes.Arity (Arity(arity))
import Data.Logic.Classes.Constants (Constants(..))
import Data.Logic.Classes.Pretty (Pretty(pretty))
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Happstack.Auth.Core.Profile   (UserId(..))
import Ontology.Types.Assertion (AssertionId, prettyAssertionId)
import Ontology.Types.Belief (Belief(..))
import Ontology.Types.DocumentId (DocumentId, prettyDocumentId)
import Ontology.Types.Subject (SubjectId, PredicateStyle(AsPredicate), prettySubjectId)
import Ontology.Types.Theorem (TheoremId, prettyTheoremId)
import Text.PrettyPrint (Doc, text, cat)
import Text.Printf (printf)

-- |The atomic predicate used as a parameter to the logic formula
-- type.  A formula is created from one of these using PredApp: @let x
-- = Users in PredApp x [Var (V "x")]@.  This is a formula with one
-- free variable, which we are calling a Predicate.
data AtomicPredicate description
    = Description Int description                     -- ^ Is the term an element of the described set?
    | Reference Int SubjectId                         -- ^ A reference to a subject, which is an n-ary predicate
                                                      -- describing a set of n-tuples.  The membership is defined
                                                      -- only by the other assertions about that subject.
    | You                                             -- ^ Is the term the currently logged in user?
    | Somebody UserId                                 -- ^ Is the term a particular user?
    | Users                                           -- ^ Is the term a user?
    | Persons                                         -- ^ Is the term a person?
    | Believers Belief                                -- ^ Is the term someone who accepts a Belief?
    | Nickname Int T.Text                             -- ^ Does not affect membership
    | NumberOf (AtomicPredicate description)          -- ^ Is the term a number which matches the cardinality of a set.
    | AssertionRef AssertionId                        -- ^ Is the term the specified assertion?
    | DocumentRef DocumentId                                -- ^ Is the term referenced in the specified document?  
    | TheoremRef TheoremId                            -- ^ Is the term the specified document?
    | Commentary Int T.Text                           -- ^ Does not affect membership
    | Singleton                                       -- ^ I'm not sure this is a meaningful predicate in first order logic.
    | Empty                                           -- ^ Empty set
    | U                                               -- ^ Universal set
    | NumberLit Double                                -- ^ Is the term the given number?
    | Ratio                                           -- ^ for triple a, b, b, true if the ratio of a : b equals c
    | PercentOf                                       -- ^ for triple a, b, c, true if a * b equals 100 * c
    deriving (Eq, Data, Typeable, Show)

instance (Eq description, Ord description, Show description) => Arity (AtomicPredicate description) where
    arity =
        Just . arity'
        where
          arity' (Description n _) = n
          arity' (Reference n _) = n
          arity' (Somebody _) = 1
          arity' You = 1
          arity' Users = 1
          arity' Persons = 1
          arity' (Believers _) = 1
          arity' (Nickname n _) = n
          arity' (NumberOf _) = 1
          arity' (AssertionRef _) = 1
          arity' (DocumentRef _) = 1
          arity' (TheoremRef _) = 1
          arity' (Commentary n _) = n
          arity' Singleton = 1
          arity' Empty = 1
          arity' U = 1
          arity' (NumberLit _) = 1
          arity' Ratio = 3
          arity' PercentOf = 3

instance Constants (AtomicPredicate description) where
    fromBool True = U
    fromBool False = Empty

{-
instance IsString AtomicPredicate where
    fromString s = error ("IsString AtomicPredicate " ++ show s)
-}

prettyAtomicPredicate :: (Eq description, Ord description, Show description) => PredicateStyle -> AtomicPredicate description -> Doc
prettyAtomicPredicate style x =
    case x of
      Reference _ ident -> prettySubjectId style ident
      AssertionRef ident -> prettyAssertionId ident
      DocumentRef ident -> prettyDocumentId ident
      TheoremRef ident -> prettyTheoremId ident
      Somebody u -> prettyUserId u
      NumberLit d -> cat [text "=", prettyNumberLit d]
      _ -> text $ show x

instance (Show description, Ord description) => Pretty (AtomicPredicate description) where
    pretty = prettyAtomicPredicate AsPredicate

prettyUserId :: UserId -> Doc
prettyUserId u = text ("U" ++ show (unUserId u))

instance Pretty UserId where
    pretty = prettyUserId

prettyNumberLit :: Double -> Doc
prettyNumberLit d = text $ let s = printf "%g" d in if isSuffixOf ".0" s then take (length s - 2) s else s

-- |Order AtomicPredicate from most to least "descriptive", meaning
-- hopefully that if you sort a list of them the one that comes first
-- best sums up the meaning of the whole list.
instance (Ord description, Show description) => Ord (AtomicPredicate description) where
    compare (NumberLit a) (NumberLit b) = compare a b
    compare (NumberLit _) _ = LT

    compare You You = EQ
    compare You _ = LT

    compare Ratio Ratio = EQ
    compare Ratio _ = LT

    compare PercentOf PercentOf = EQ
    compare PercentOf _ = LT

    compare (Somebody a) (Somebody b) = compare a b
    compare (Somebody _) _ = LT

    compare (Reference _ a) (Reference _ b) = compare a b
    compare (Reference _ _) _ = LT

    compare (AssertionRef a) (AssertionRef b) = compare a b
    compare (AssertionRef _) _ = LT

    compare (TheoremRef a) (TheoremRef b) = compare a b
    compare (TheoremRef _) _ = LT

    compare (Nickname _ a) (Nickname _ b) = compare a b
    compare (Nickname _ _) _ = LT

    compare (Description _ a) (Description _ b) = compare a b
    compare (Description _ _) _ = LT

    compare Persons Persons = EQ
    compare Persons _ = LT

    compare Users Users = EQ
    compare Users _ = LT

    compare (DocumentRef a) (DocumentRef b) = compare a b
    compare (DocumentRef _) _ = LT

    compare (NumberOf a) (NumberOf b) = compare a b
    compare (NumberOf _) _ = LT

    compare Empty Empty = EQ
    compare Empty _ = LT

    compare Singleton Singleton = EQ
    compare Singleton _ = LT

    compare U U = EQ
    compare U _ = LT

    compare (Commentary _ a) (Commentary _ b) = compare a b
    compare (Commentary _ _) _ = LT

    compare (Believers a) (Believers b) = compare a b
    compare (Believers _) _ = LT
{-
    compare (Somebody a) (Somebody b) = compare a b
    compare (Reference _ a) (Reference _ b) = compare a b
    compare (AssertionRef a) (AssertionRef b) = compare a b
    compare (DocumentRef a) (DocumentRef b) = compare a b
    compare (TheoremRef a) (TheoremRef b) = compare a b
    compare (Nickname _ a) (Nickname _ b) = compare a b
    compare (Description _ a) (Description _ b) = compare a b
    compare (NumberOf a) (NumberOf b) = compare a b
    compare (Commentary _ a) (Commentary _ b) = compare a b
    compare (Believers a) (Believers b) = compare a b
    compare a b = compare (specificity a) (specificity b)

-- |Classify how useful a predicate is in characterizing a subject,
-- with 1 being the most useful and higher numbers less so.
specificity :: (Eq description, Ord description, Show description) => AtomicPredicate description -> Int
specificity (NumberLit _) = 1
specificity You = 2
specificity Ratio = 3
specificity PercentOf = 4
specificity (Somebody _) = 5
specificity (Reference _ _) = 6
specificity (AssertionRef _) = 7
specificity (TheoremRef _) = 8
specificity (Nickname _ _) = 9
specificity (Description _ _) = 10
specificity Persons = 11
specificity Users = 12
specificity (DocumentRef _) = 13
specificity (NumberOf _) = 14
specificity Empty = 15
specificity Singleton = 16
specificity U = 17
specificity (Commentary _ _) = 18
specificity (Believers _) = 19
-}

$(deriveSafeCopy 1 'base ''AtomicPredicate)
