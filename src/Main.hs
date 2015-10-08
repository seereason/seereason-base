-- | This is a program which demonstrates pulling data out of the
-- database in JSON format, converting it back to haskell types, and
-- doing something with it.
{-# LANGUAGE FlexibleContexts, PackageImports, ScopedTypeVariables #-}
module Main where

import Control.Applicative ((<$>))
import qualified Codec.Binary.UTF8.String as UTF8
import "mtl" Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as L
import Data.Generics (gshow)
import Data.List (intercalate)
import Data.Logic.Classes.Combine (Combination(..), BinOp(..))
import Data.Logic.Classes.FirstOrder (foldFirstOrder, Quant(..))
import Data.Logic.Classes.Pretty (Pretty(pPrint))
import Data.Logic.Classes.Term (foldTerm)
import Data.Logic.Types.FirstOrder (Predicate(..))
import qualified Data.Text as Text
import GenI.URL (GenIURL(..))
import Data.UserId (UserId(UserId))
import JSON.Generic2 (decodeJSON)
import Network.HTTP.Conduit (simpleHttp)
import Ontology.Types (SubjectId(..), prettySubjectId, unsafeSubjectId, unsafeAssertionId, SubjectNode(..), Assertion(..), AssertionId(..),
                       PredicateStyle(AsPredicate, AsFunction))
import Ontology.Types.Formula.AtomicFunction (AtomicFunction(Function))
import Ontology.Types.Formula.AtomicPredicate (AtomicPredicate(Reference, Description))
import Ontology.Types.GenI (LSubject(..))
import Ontology.Types.PF (FormulaPF)
import System.IO (putStrLn)
import URL
import Web.Routes (RouteT(unRouteT), setDefault, showURL,
                   Site(Site, handleSite, formatPathSegments, parsePathSegments), toPathSegments, toPathInfoParams, parseSegments, fromPathSegments)
import Web.Routes.RouteT (MonadRoute, RouteT(..), runRouteT)
import Web.Routes.Site (runSite)

main = run $
    do url <- Text.unpack <$> showURL (GenI_Assertion (unsafeAssertionId 4))
       resp <- liftIO $ simpleHttp url
       let a4 = decodeJSON . UTF8.decode . L.unpack $ resp :: Assertion FormulaPF
       text <- geni (proposition a4)
       liftIO (putStrLn text)

run :: RouteT GenIURL m a -> m a  
run route =
  unRouteT route showFn
  where
    showFn url params = (Text.pack "http://seereason.com/web") `Text.append` (toPathInfoParams (W_GenI url) params)

askAssertion :: AssertionId -> RouteT GenIURL IO (Assertion FormulaPF)
askAssertion i =
    do url <- Text.unpack <$> showURL (GenI_Assertion i)
       resp <- liftIO $ simpleHttp url
       let assert = decodeJSON . UTF8.decode . L.unpack $ resp :: Assertion FormulaPF
       return assert

askSubject :: Maybe UserId -> SubjectId -> RouteT GenIURL IO (LSubject FormulaPF)
askSubject u i =
    do url  <- Text.unpack <$> showURL (maybe GenI_Subject GenI_UserSubject u $ i)
       resp <- liftIO $ simpleHttp url
       let subj = decodeJSON .  UTF8.decode . L.unpack $ resp :: LSubject FormulaPF
       return subj

askDescriptions :: Maybe UserId -> SubjectId -> RouteT GenIURL IO [String]
askDescriptions u i =
    do url  <- Text.unpack <$> showURL (maybe GenI_Descriptions GenI_UserDescriptions u $ i)
       resp <- liftIO $ simpleHttp url
       let descs = decodeJSON .  UTF8.decode . L.unpack $ resp :: [String]
       return descs

geni :: FormulaPF -> RouteT GenIURL IO String
geni f =
    foldFirstOrder qu co tf at f
    where
      qu Forall v f =
          do text <- geni f
             return $ "for all " ++ show (pPrint v) ++ " " ++ text
      qu Exists v f =
          do text <- geni f
             return $ "for all " ++ show (pPrint v) ++ " " ++ text
      co (BinOp f1 op f2) =
          do t1 <- geni f1
             t2 <- geni f2
             let op' = case op of
                         (:<=>:) -> "if and only if"
                         (:=>:) -> "implies"
                         (:&:) -> "and"
                         (:|:) -> "or"
             return $ t1 ++ " " ++ op' ++ " " ++ t2
      co ((:~:) f) =
          do text <- geni f
             return $ "not " ++ text
      tf True = return "true"
      tf False = return "false"
      at (Equal t1 t2) =
          do text1 <- geniTerm t1
             text2 <- geniTerm t2
             return $ text1 ++ " equals " ++ text2
      at (Apply pr ts) =
          do prtext <- geniPred pr
             termTexts <- mapM geniTerm ts
             return $ "apply " ++ prtext ++ " to (" ++ intercalate ", " termTexts ++ ")"

geniTerm t =
    foldTerm var fn t
    where
      var v = return $ "variable " ++ show (pPrint v)
      fn f ts =
          do termTexts <- mapM geniTerm ts
             ftext <- geniFunction f
             return $ "apply function " ++ ftext ++ " to (" ++ intercalate ", " termTexts ++ ")"

geniPred (Reference arity i) =
    do descs <- askDescriptions (Just (UserId 1)) i
       case descs of
         [] -> return $ show $ prettySubjectId AsPredicate i
         (desc : _) -> return desc
geniPred x = return $ show . pPrint $ x

-- | Not exactly
geniFunction (Function (Reference arity i)) =
    do descs <- askDescriptions (Just (UserId 1)) i
       case descs of
         [] -> return $ show $ prettySubjectId AsFunction i
         (desc : _) -> return desc

test0 :: RouteT GenIURL IO String
test0 =
    do a5 <- askAssertion (unsafeAssertionId 5)
       geni (proposition a5)

test1 :: RouteT GenIURL IO ()
test1 = 
  do url  <- Text.unpack <$> showURL (GenI_UserSubject (UserId 1) (unsafeSubjectId 52))
     liftIO $ putStrLn url

test2 :: RouteT GenIURL IO ()
test2 =
  do url  <- Text.unpack <$> showURL (GenI_UserSubject (UserId 1) (unsafeSubjectId 52))
     resp <- liftIO $ simpleHttp url
     let subj = decodeJSON .  UTF8.decode . L.unpack $ resp :: LSubject FormulaPF
     liftIO $ print (map unSubjectNode . subjectIds $ subj)
     return ()

-- | Usage examples of the other requests:
tests :: IO ()
tests =
   do run (Text.unpack <$> showURL GenI_Subjects) >>= simpleHttp >>= putStrLn . show . map pPrint . (decodeJSON :: String -> [SubjectId]) . UTF8.decode . L.unpack
      run (Text.unpack <$> showURL GenI_Assertions) >>= simpleHttp >>= putStrLn . show . map pPrint . (decodeJSON :: String -> [Assertion FormulaPF]) . UTF8.decode . L.unpack
      run (Text.unpack <$> showURL (GenI_Assertion (unsafeAssertionId 1))) >>= simpleHttp >>= putStrLn . show . pPrint . (decodeJSON :: String -> Assertion FormulaPF) . UTF8.decode . L.unpack
      run (Text.unpack <$> showURL (GenI_UserSubject (UserId 1) (unsafeSubjectId 52))) >>= simpleHttp >>=  putStrLn . show . subjectArity . (decodeJSON :: String -> LSubject FormulaPF) . UTF8.decode . L.unpack
