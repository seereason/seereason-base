{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Ontology.Types.PredForm
    ( -- * PredForm
      PredForm(PredForm)
    , foldPred
    , makePred
    ) where

import Data.Data (Data(..))
import Data.Logic.Classes.Apply (pApp)
import Data.Logic.Classes.Arity (Arity(arity))
import Data.Logic.Classes.Constants (fromBool)
import Data.Logic.Classes.Equals (HasEquality, foldAtomEq)
import Data.Logic.Classes.FirstOrder (IsQuantified(foldQuantified))
import Data.Logic.Classes.Term (IsTerm(vt))
import Data.Logic.Classes.Variable (variants)
import Data.SafeCopy -- (base, extension, deriveSafeCopy)
import Data.String (IsString(fromString))
import Data.Typeable (Typeable)

-- |A PredForm is a wrapper around a formula that indicates that
-- the formula is of the form Apply p [term].
newtype PredForm formula = PredForm formula
    deriving (Eq, Ord, Data, Typeable, Read, Show)

-- ^ This function is used to access the predicate in a PredForm.
-- Note that the type "a" might be a function such as "[term] -> b",
-- which means you can simulate the pApp function.
foldPred :: (IsQuantified formula atom v, HasEquality atom p term) => (p -> a) -> PredForm formula -> a
foldPred fn (PredForm form) =
    foldQuantified qu co tf at form
    where
      at = foldAtomEq (\ p _ -> fn p) (\ _ _ -> undefined)
      tf = fn . fromBool
      qu = undefined
      co = undefined

-- |Create a PredForm from an atomic predicate and some generated terms.
makePred :: (IsQuantified formula atom v, HasEquality atom p term, IsTerm term v f) => p -> PredForm formula
makePred p = PredForm (pApp p ts)
    where ts = case arity p of
                 Nothing -> error "makePred: Fixed arity expected"
                 Just n -> take n (map vt (variants (fromString "x")))

instance (IsQuantified formula atom v, HasEquality atom p term) => Arity (PredForm formula) where
    arity = foldPred arity

$(deriveSafeCopy 1 'base ''PredForm)
