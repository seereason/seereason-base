-- | This is a program which demonstrates pulling data out of the
-- database in JSON format, converting it back to haskell types, and
-- doing something with it.
{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
module Main where

import Control.Applicative ((<$>))
import qualified Codec.Binary.UTF8.String as UTF8
import "mtl" Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as Text
import GenI.URL (GenIURL(..))
import Happstack.Auth.Core.Profile (UserId(UserId))
import JSON.Generic2 (decodeJSON)
import Network.HTTP.Enumerator (simpleHttp)
import Ontology.Types (SubjectId(..), unsafeSubjectId, unsafeAssertionId, SubjectNode(..))
import Ontology.Types.GenI (LSubject(..))
import Ontology.Types.PF (FormulaPF)
import System.IO (putStrLn)
import URL
import Web.Routes (RouteT(unRouteT), setDefault, showURL,
                   Site(Site, handleSite, formatPathSegments, parsePathSegments), toPathSegments, toPathInfoParams, parseSegments, fromPathSegments)
import Web.Routes.RouteT (MonadRoute, RouteT(..), runRouteT)
import Web.Routes.Site (runSite)

main :: IO ()
main = run test2
  
run :: RouteT GenIURL m a -> m a  
run route =
  unRouteT route showFn
  where
    showFn url params = (Text.pack "http://gene.local:8000/web") `Text.append` (toPathInfoParams (W_GenI url) params)
    
test1 :: RouteT GenIURL IO ()
test1 = 
  do url  <- Text.unpack <$> showURL (GenI_Subject (UserId 1) (unsafeSubjectId 52))
     liftIO $ putStrLn url

test2 :: RouteT GenIURL IO ()
test2 =
  do url  <- Text.unpack <$> showURL (GenI_Subject (UserId 1) (unsafeSubjectId 52))
     resp <- liftIO $ simpleHttp url
     let subj = decodeJSON .  UTF8.decode . L.unpack $ resp :: LSubject FormulaPF
     liftIO $ print (map unSubjectNode . subjectIds $ subj)
     return ()
