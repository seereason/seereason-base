{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module Ontology.Types.UserData
    ( UserData(UserData, insertMode, subjectSet, theme)
    , Theme(..)
    ) where

import Data.Generics (Data, Typeable)
import qualified Data.Map as Map
import Data.SafeCopy (SafeCopy, base, extension, deriveSafeCopy, Migrate(..))
import qualified Data.Set as Set
import qualified Data.Text as T
import Ontology.Types (SubjectId, DocumentId)
import Ontology.Types.InsertMode (InsertMode)

-- | This data should be moved out of the SeeReason ixset and into the
-- Preferences ixset.  This might let us remove the userdata type
-- parameter from the SeeReason type in the ontology package.
data UserData formula
    = UserData
      { insertMode :: InsertMode formula
      -- ^ The mode under which new assertions are added to the database.
      , subjectSet :: Set.Set SubjectId
      -- ^ The user's working set subjects, to populate a menu of subjects
      -- which can be used when constructing propositions.
      , theme :: Theme
      } deriving (Typeable)

data Theme = DefaultTheme | IPadTheme deriving (Typeable)

$(deriveSafeCopy 2 'extension ''UserData)
$(deriveSafeCopy 1 'base ''Theme)

-- Migration

data UserData_v1 formula
    = UserData_v1
      { insertMode_v1 :: InsertMode formula
      -- ^ The mode under which new assertions are added to the database.
      , subjectSet_v1 :: Set.Set SubjectId
      -- ^ The user's working set subjects, to populate a menu of subjects
      -- which can be used when constructing propositions.
      , _remarks :: Map.Map DocumentId T.Text
      } deriving (Typeable)

$(deriveSafeCopy 1 'base ''UserData_v1)

instance (SafeCopy formula, Data formula, Typeable formula, Ord formula) => Migrate (UserData formula) where
    type MigrateFrom (UserData formula) = UserData_v1 formula
    migrate x@(UserData_v1 {}) = UserData { insertMode = insertMode_v1 x
                                          , subjectSet = subjectSet_v1 x
                                          -- Discarding old remarks field, not a big loss
                                          , theme = IPadTheme }
