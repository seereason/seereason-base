{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS -Wall -Wwarn #-}
module Ontology.Types.Formula.AtomicPredicate
    ( AtomicPredicate(..)
    , prettyAtomicPredicate
    , prettyUserId
    , prettyNumberLit
    , compare2
    , Ordering2(..)
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

-- | An Ordering2 represents two levels of comparison, comparison of
-- | just the constructor and comparison of the whole value.
data Ordering2 = EQ2 Ordering | LT2 | GT2

-- | Return two measures of ordering, on the constructor only and on
-- the entire value.
compare2 :: (Show description, Ord description) => AtomicPredicate description -> AtomicPredicate description -> Ordering2
compare2 p1 p2 =
    case (p1, p2) of
      (NumberLit a, NumberLit b) -> EQ2 (compare a b)
      (NumberLit _, _) -> LT2
      (_, NumberLit _) -> GT2

      (You, You) -> EQ2 EQ
      (You, _) -> LT2
      (_, You) -> GT2

      (Ratio, Ratio) -> EQ2 EQ
      (Ratio, _) -> LT2
      (_, Ratio) -> GT2

      (PercentOf, PercentOf) -> EQ2 EQ
      (PercentOf, _) -> LT2
      (_, PercentOf) -> GT2

      (Somebody _, Somebody b) -> EQ2 EQ
      (Somebody _, _) -> LT2
      (_, Somebody _) -> GT2

      (Reference _ a, Reference _ b) -> EQ2 $ compare a b
      (Reference _ _, _) -> LT2
      (_, Reference _ _) -> GT2

      (AssertionRef a, AssertionRef b) -> EQ2 $ compare a b
      (AssertionRef _, _) -> LT2
      (_, AssertionRef _) -> GT2

      (TheoremRef a, TheoremRef b) -> EQ2 $ compare a b
      (TheoremRef _, _) -> LT2
      (_, TheoremRef _) -> GT2

      (Nickname _ a, Nickname _ b) -> EQ2 $ compare a b
      (Nickname _ _, _) -> LT2
      (_, Nickname _ _) -> GT2

      (Description _ a, Description _ b) -> EQ2 $ compare a b
      (Description _ _, _) -> LT2
      (_, Description _ _) -> GT2

      (Persons, Persons) -> EQ2 EQ
      (Persons, _) -> LT2
      (_, Persons) -> GT2

      (Users, Users) -> EQ2 EQ
      (Users, _) -> LT2
      (_, Users) -> GT2

      (DocumentRef a, DocumentRef b) -> EQ2 $ compare a b
      (DocumentRef _, _) -> LT2
      (_, DocumentRef _) -> GT2

      (NumberOf a, NumberOf b) -> EQ2 $ compare a b
      (NumberOf _, _) -> LT2
      (_, NumberOf _) -> GT2

      (Empty, Empty) -> EQ2 EQ
      (Empty, _) -> LT2
      (_, Empty) -> GT2

      (Singleton, Singleton) -> EQ2 EQ
      (Singleton, _) -> LT2
      (_, Singleton) -> GT2

      (U, U) -> EQ2 EQ
      (U, _) -> LT2
      (_, U) -> GT2

      (Commentary _ a, Commentary _ b) -> EQ2 $ compare a b
      (Commentary _ _, _) -> LT2
      (_, Commentary _ _) -> GT2

      (Believers a, Believers b) -> EQ2 $ compare a b
      (Believers _, _) -> LT2
      (_, Believers _) -> GT2
      

-- |Order AtomicPredicate from most to least "descriptive", meaning
-- hopefully that if you sort a list of them the one that comes first
-- best sums up the meaning of the whole list.
instance (Ord description, Show description) => Ord (AtomicPredicate description) where
    compare a b =
        case compare2 a b of
          LT2 -> LT
          GT2 -> GT
          EQ2 x -> x

$(deriveSafeCopy 1 'base ''AtomicPredicate)
