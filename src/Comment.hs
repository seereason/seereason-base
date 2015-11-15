{-# LANGUAGE DeriveDataTypeable, FunctionalDependencies, MultiParamTypeClasses, TemplateHaskell #-}
module Comment
    ( MkURL(..)
    , URL(..)
    ) where

import Data.Data
import CommentTypes (CommentId(..))
import Test.QuickCheck     (Arbitrary(..),oneof)
import Web.Routes.TH       (derivePathInfo)

data URL topic
    = Comment CommentId
    | Submit topic
    | Spam CommentId
    deriving (Eq, Ord, Read, Show, Data, Typeable)

class MkURL topic url | url -> topic where
    mkURL :: URL topic -> url
    topicURL :: topic -> url

$(derivePathInfo ''URL)
instance Arbitrary topic => Arbitrary (URL topic) where
    arbitrary =
        oneof [ Comment . CommentId <$> arbitrary
              , Submit <$> arbitrary
              , Spam . CommentId <$> arbitrary ]
