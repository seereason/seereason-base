{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, UndecidableInstances #-}
{-# OPTIONS -Wall -Wwarn #-}
module Ontology.Types.Formula.V
    ( V(..)
    , prettyV
    ) where

import Data.Char (isDigit)
import Data.Generics (Data, Typeable)
import Data.Logic.Classes.Pretty (Pretty(pPrint))
import Data.Logic.Classes.Variable (Variable(..))
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Set as Set
import Data.String (IsString(..))
import Text.PrettyPrint.HughesPJClass (Doc, text)

-- | Variable names
newtype V = V String
    deriving (Eq, Ord, Typeable, Data, Show) -- Monoid,IsString

instance Variable V where
    variant x xs =
        if Set.member x xs
        then variant (next x) xs
        else x
        where
          next :: V -> V
          next (V s) =
              V (case break (not . isDigit) (reverse s) of
                   (_, "") -> "x"
                   ("", nondigits) -> nondigits ++ "2"
                   (digits, nondigits) -> nondigits ++ show (1 + read (reverse digits) :: Int))
    prefix p (V s) = V (p ++ s)
    prettyVariable (V s) = text s

-- instance Show V where
    -- It looks more like logic with out the quotes, but you can't
    -- paste it back into your haskell program.
--     show (V s) = show s

prettyV :: V -> Doc
prettyV (V s) = text s

instance Pretty V where
    pPrint = prettyV

instance IsString V where
    fromString = V

$(deriveSafeCopy 1 'base ''V)

-- |We need a Default instance here because we use this in a generic formlet.
-- When we eliminate g-f we can remove this.
-- $(deriveNewData [''AtomicPredicate, ''AtomicFunction])
