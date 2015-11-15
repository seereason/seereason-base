{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Preferences.URL
       ( URL(..)
       , MkURL(..)
       ) where

import Data.Data
import Data.UserId (UserId(..))
import Test.QuickCheck     (Arbitrary(..),oneof)
import Web.Routes.TH       (derivePathInfo)

data URL
    = CreateNew
    | View UserId
    | Edit
      deriving (Eq, Ord, Read, Show, Data, Typeable)

class MkURL url where
  mkURL :: URL -> url

$(derivePathInfo ''URL)
instance Arbitrary URL where
    arbitrary =
        oneof [ return CreateNew
              , View . UserId <$> arbitrary
              , return Edit
              ]
