{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Ontology.Types.PredForm
    ( -- * PredForm
      PredForm(PredForm)
    , foldPred
    , makePred
    ) where

import Data.Data (Data(..))
import Data.Logic.Classes.Arity (Arity(arity))
import Data.Logic (Term(vt), pApp, Predicate(Apply))
import Data.Logic.Classes.FirstOrder (FirstOrderFormula(foldFirstOrder))
import Data.Logic.Classes.Variable (Variable(..), variants)
import Data.SafeCopy -- (base, extension, deriveSafeCopy)
import Data.Typeable (Typeable)
import Happstack.Data (deriveNewData)

-- |A PredForm is a wrapper around a formula that indicates that
-- the formula is of the form Apply p [term].
newtype PredForm formula = PredForm formula
    deriving (Eq, Ord, Data, Typeable, Read, Show)

-- ^ This function is used to access the predicate in a PredForm.
-- Note that the type "a" might be a function such as "[term] -> b",
-- which means you can simulate the pApp function.
foldPred :: FirstOrderFormula formula term v p f => (p -> a) -> PredForm formula -> a
foldPred fn (PredForm form) =
    foldFirstOrder q c p form
    where
      -- 
      p (Apply ap _) = fn ap
      -- We don't have to implement anything else, because we know
      -- this first case will match.
      p _ = undefined
      q = undefined
      c = undefined

-- |Create a PredForm from an atomic predicate and some generated terms.
makePred :: FirstOrderFormula formula term v p f => p -> PredForm formula
makePred p = PredForm (pApp p ts)
    where ts = case arity p of
                 Nothing -> error "makePred: Fixed arity expected"
                 Just n -> take n (map vt (variants (fromString "x")))

instance FirstOrderFormula formula term v p f => Arity (PredForm formula) where
    arity = foldPred arity

$(deriveSafeCopy 1 'base ''PredForm)

$(deriveNewData [''PredForm])
