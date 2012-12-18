{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, UndecidableInstances #-}
module Ontology.Types.InsertMode where

import Data.Generics (Typeable)
import Data.SafeCopy (deriveSafeCopy, base)
import Ontology.Types.PredForm (PredForm)

data InsertMode formula
    = NoOpinion
      -- ^ In this mode, no extra assertions are added with each
      -- assertion.
    | Accepting
      -- ^ In this mode, when an assertion is added, a second
      -- assertion is inserted stating that the current user also
      -- accepts the assertion.
    | Authoring (PredForm formula)
      -- ^ In this mode an extra assertion is added saying that the
      -- author predicate (the argument) accepts the assertion.
      -- Accepting mode is equivalent to Authoring (PredForm You).
    | Certifying (PredForm formula)
      -- ^ A combination of Authoring and Accepting.  That is to say,
      -- the current user "certifies" his or her analysis of the
      -- author's text.  The predicate matches the document authors,
      -- whose work the user is certifying.
    deriving (Typeable, Show)

$(deriveSafeCopy 1 'base ''InsertMode)
