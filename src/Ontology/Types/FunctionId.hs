{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Ontology.Types.FunctionId
    ( FunctionId(unFunctionId)
    , prettyFunctionId
    , unsafeFunctionId
    ) where

import Data.Data (Data(..))
import Data.Typeable (Typeable)
import Text.JSON (JSON(readJSON, showJSON), makeObj, valFromObj, JSValue(JSObject))
import Text.PrettyPrint (Doc, text)

-- |A type to identify functions defined using the ontology mechanism
-- the same way subjects are.
newtype FunctionId = FunctionId {unFunctionId :: Integer} deriving (Data, Typeable, Eq, Ord, Read)

instance Show FunctionId where
    show x = "(unsafeFunctionId " ++ show (unFunctionId x) ++ ")"

prettyFunctionId :: FunctionId -> Doc
prettyFunctionId x = text ("F" ++ show (unFunctionId x))

unsafeFunctionId :: Integer -> FunctionId
unsafeFunctionId = FunctionId

instance JSON FunctionId where
    showJSON sid = makeObj [ ("functionId", showJSON (unFunctionId sid)) ]
    readJSON (JSObject jsobj) = unsafeFunctionId <$> valFromObj "functionId" jsobj
    readJSON _ = error "Unexpected JSON for FunctionId"
