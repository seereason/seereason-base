{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Ontology.Types.Theorem
    ( prettyUserId
    -- * TheoremId, Theorem
    , Theorem(Theorem, theoremOwner, theoremId, argument, theoremPrivacy)
    , TheoremId(unTheoremId)
    , findTheoremIds
    , unsafeTheoremId
    , prettyTheoremId
    , Theorems
    ) where

import Data.Data (Data(..))
import Data.Function (on)
import Data.IxSet (inferIxSet, noCalcs)
import Data.SafeCopy -- (base, extension, deriveSafeCopy)
import Data.Monoid ((<>))
import qualified Data.Set.Extra as Set
import Data.Typeable (Typeable)
import Data.UserId (UserId(..))
import Ontology.Types.Assertion (AssertionId, PrivacyState(Proposed))
import Test.QuickCheck (Arbitrary(arbitrary))
import Text.PrettyPrint (Doc, text, hang)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), vcat)
import Web.Routes.TH (derivePathInfo)

prettyUserId :: UserId -> Doc
prettyUserId u = text ("U" ++ show (_unUserId u))

instance Pretty UserId where
    pPrint = prettyUserId

instance Pretty TheoremId where
    pPrint (TheoremId n) = pPrint "(TheoremId " <> text (show n ++ ")")

-- | A Theorem is a list of assertions.  These are passed to
-- the theorem prover.
data Theorem =
    Theorem { theoremOwner :: UserId
            , theoremId :: TheoremId
            , argument :: [AssertionId]
            , theoremPrivacy :: PrivacyState
            } deriving (Data, Typeable, Show)

instance Pretty Theorem where
    pPrint Theorem{..} =
        hang (text "Threorem") 1
             (vcat [text "theoremOwner: " <> pPrint theoremOwner,
                    text "theoremId: " <> pPrint theoremId,
                    text "argument: " <> pPrint argument,
                    text "theoremPrivacy: " <> pPrint theoremPrivacy])

-- | Only one theorem including a given list of assertions can be
-- entered into the system.  (Should this be a set?  Or a set and
-- a conclusion assertion?)
instance Ord Theorem where
    compare = compare `on` argument

instance Eq Theorem where
    a == b = compare a b == EQ

data TheoremId = TheoremId {unTheoremId :: Integer} deriving (Read, Eq, Ord, Data, Typeable)

instance Show TheoremId where
    show x = "(unsafeTheoremId " ++ show (unTheoremId x) ++ ")"

$(derivePathInfo ''TheoremId)

prettyTheoremId :: TheoremId -> Doc
prettyTheoremId x = text ("T" ++ show (unTheoremId x))

unsafeTheoremId :: Integer -> TheoremId
unsafeTheoremId = TheoremId

findTheoremIds :: Data a => a -> Set.Set TheoremId
findTheoremIds a = Set.gFind a

instance Arbitrary TheoremId where
    arbitrary = (TheoremId <$> arbitrary)

$(deriveSafeCopy 2 'extension ''Theorem)
$(deriveSafeCopy 1 'base ''TheoremId)

{-
instance (New.Data DefaultD Theorem) where 
    toConstr = error "toConstr Ontology.Types.Theorem"

instance Default Theorem where
    defaultValue = Theorem { theoremOwner = UserId 0
                           , theoremId    = defaultValue
                           , argument     = defaultValue
                           , theoremPrivacy = Proposed
                           }

-- It would be better not to have a Default instance for the Id types
$(deriveNewDataNoDefault [''TheoremId])

instance Default TheoremId where
    defaultValue = TheoremId 1
-}

$(inferIxSet "Theorems" ''Theorem 'noCalcs [''TheoremId, ''AssertionId])

-- Migration

data Theorem_v1 =
    Theorem_v1 { theoremOwner_v1 :: UserId
               , theoremId_v1 :: TheoremId
               , argument_v1 :: [AssertionId]
               } deriving (Data, Typeable)

$(deriveSafeCopy 1 'base ''Theorem_v1)

instance Migrate Theorem where
    type MigrateFrom Theorem = Theorem_v1
    migrate x@(Theorem_v1 {}) = Theorem { theoremOwner = theoremOwner_v1 x,
                                          theoremId = theoremId_v1 x,
                                          argument = argument_v1 x,
                                          theoremPrivacy = Proposed }
