{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
 module Ontology.Types.Belief
     ( Belief(Belief, theQuestion, theAnswer, startTime)
     , Answer(Answer, Nonsense)
     ) where

import Data.Data (Data(..))
import Data.Function (on)
import Data.Logic.Classes.Constants (prettyBool)
import Data.Logic.Classes.Pretty (Pretty(pretty))
import Data.Logic.KnowledgeBase (ProofResult(..))
import Data.SafeCopy -- (base, extension, deriveSafeCopy)
import Data.Time.Clock (UTCTime(..))
import Data.Typeable (Typeable)
import Happstack.Data (Default(defaultValue), deriveNewDataNoDefault)
import Ontology.Types.Assertion (AssertionId)
import Text.PrettyPrint (text, (<>))

-- |A belief is the association of an assertion with a truth value
-- indicating whether the assertion is accepted, rejected, or that one
-- has no opinion.  The Believers predicate above is used to construct
-- a set of people who accept or reject an assertion.  The user is
-- considered to have this belief about the assertion starting from
-- the time the belief is created until the creation of a new belief about
-- the same assertion.  This is why the truth value is a maybe, so that
-- a user who accepted or rejected an assertion to later have no opinion.
data Belief
    = Belief { theQuestion :: AssertionId -- You believe this assertion is...
             , theAnswer :: Answer        -- always true, always false, or could be either...
             , startTime :: UTCTime       -- as of this time.
             } deriving (Data, Typeable, Show)

data Answer
    = Answer ProofResult
    | Nonsense
    deriving (Eq, Ord, Data, Typeable, Show)

instance Ord Belief where
    compare a b =
        foldl (\ ordering cmp -> if ordering == EQ then cmp a b else ordering) EQ
                  [compare `on` theQuestion,
                   flip (compare `on` startTime),
                   compare `on` theAnswer]

instance Eq Belief where
    a == b = compare a b == EQ

instance Pretty Belief where
    pretty b = pretty (theQuestion b) <> text " is " <> pretty (theAnswer b) <> text " as of " <> text (show (startTime b))

instance Pretty Answer where
    pretty (Answer Proved) = prettyBool True
    pretty (Answer Disproved) = prettyBool False
    pretty (Answer Invalid) = text "Contingent"
    pretty Nonsense = text "Nonsense"

$(deriveSafeCopy 2 'extension ''Belief)
$(deriveSafeCopy 1 'base ''Answer)

$(deriveNewDataNoDefault [''Belief, ''Answer])

instance Default Answer where
    defaultValue = Answer defaultValue

-- Migration

data Belief_v1
    = Belief_v1
      { theQuestion_v1 :: AssertionId
      , theAnswer_v1 :: Maybe Bool
      , startTime_v1 :: UTCTime
      } deriving (Data, Typeable, Show)

$(deriveSafeCopy 1 'base ''Belief_v1)

instance Migrate Belief where
    type MigrateFrom Belief = Belief_v1
    migrate x@(Belief_v1 {}) = Belief {theQuestion = theQuestion_v1 x,
                                       theAnswer = maybe (Answer Invalid) (\ flag -> Answer (if flag then Proved else Disproved)) (theAnswer_v1 x),
                                       startTime = startTime_v1 x}
