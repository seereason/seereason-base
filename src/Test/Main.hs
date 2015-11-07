{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, PackageImports, StandaloneDeriving, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Wwarn #-}
module Main where

import Data.Logic.Classes.Atom (Atom(..))
import Data.Logic.KnowledgeBase (WithId(WithId, wiItem, wiIdent))
import Data.Logic.Normal.Implicative (ImplicativeForm(INF, neg, pos), implicativeNormalForm {-, runNormal-})
import Data.Logic.Resolution (getSubstAtomEq, isRenameOfAtomEq)
import Data.Logic.Resolution (SetOfSupport, prove)
import Data.Logic.Types.FirstOrder as N
import Data.Logic.Types.FirstOrderPublic (markPublic)
import qualified Data.Map as Map
import Data.Set.Extra as Set (empty, fromList, map, Set)
import FOL (asubst, fva, foldEquate, HasApply(PredOf, TermOf), IsTerm(..), (.=.), pApp, V(V))
import Formulas (atomic, IsFormula(AtomOf))
import Ontology.Types.Description (Description)
import Ontology.Types.Formula.AtomicFunction (AtomicFunction(..))
import Ontology.Types.Formula.AtomicPredicate (AtomicPredicate(..))
import Ontology.Types.Formula (LiteralF, TermF)
import Ontology.Types.PF (FormulaPF, LiteralPF)
import Ontology.Types (unsafeSubjectId, unsafeAssertionId)
import Prelude hiding (negate)
import Prop (PFormula)
import Skolem (runSkolem, skolemize, SkolemM)
import System.Exit
import Test.HUnit

instance Atom (NPredicate (AtomicPredicate Description) (NTerm V (AtomicFunction Description V)))
              (NTerm V (AtomicFunction Description V))
              V where
    substitute = asubst
    freeVariables = fva
    allVariables = fva -- Variables are always free in an atom - this method is unnecessary
    unify = unify
    match = unify
    foldTerms f r pr = foldEquate (\t1 t2 -> f t2 (f t1 r)) (\_ ts -> Prelude.foldr f r ts) pr
    isRename = isRenameOfAtomEq
    getSubst = getSubstAtomEq

instance Atom (NPredicate (AtomicPredicate String) (NTerm V (AtomicFunction String V)))
              (TermF String)
              V where
    substitute = asubst
    freeVariables = fva
    allVariables = fva -- Variables are always free in an atom - this method is unnecessary
    unify = unify
    match = unify
    foldTerms f r pr = foldEquate (\t1 t2 -> f t2 (f t1 r)) (\_ ts -> Prelude.foldr f r ts) pr
    isRename = isRenameOfAtomEq
    getSubst = getSubstAtomEq

pApp1 :: (IsFormula formula, HasApply atom, atom ~ AtomOf formula, predicate ~ PredOf atom, term ~ TermOf atom) => predicate -> term -> formula
pApp1 p a = pApp p [a]
pApp2 :: (IsFormula formula, HasApply atom, atom ~ AtomOf formula, predicate ~ PredOf atom, term ~ TermOf atom) => predicate -> term -> term -> formula
pApp2 p a b = pApp p [a, b]

main :: IO ()
main =
    runTestTT (TestList [prove1, prove2, atomic2, atomic3, atomic4, atomic5]) >>= doCounts
    where
      doCounts counts' = exitWith (if errors counts' /= 0 || failures counts' /= 0 then ExitFailure 1 else ExitSuccess)

-- prove :: Literal lit term v p f => SetOfSupport lit v term -> SetOfSupport lit v term -> S.Set (ImplicativeForm lit) -> (Bool, SetOfSupport lit v term)

-- type Description = String

prove1 :: Test
prove1 =
    TestCase (assertEqual "Prover bug 1" expected (prove (Just 20) Set.empty sos (Set.map wiItem kb)))
    where
      x = vt (V "x")
      s49 = pApp1 (Reference 1 (unsafeSubjectId 49))
      s51 = pApp2 (Reference 2 (unsafeSubjectId 51))
      s52 = pApp1 (Reference 1 (unsafeSubjectId 52))
      s54 = pApp1 (Reference 1 (unsafeSubjectId 54))
      s58 = pApp1 (Reference 1 (unsafeSubjectId 58))
      f53 = fApp (Function (Reference 3 (unsafeSubjectId 53)))
      a155 = pApp1 (AssertionRef (unsafeAssertionId 155))
      sk1 = fApp (Skolem "1" 1)
      n1 = fApp (Function (NumberLit 1.0))
      n2 = fApp (Function (NumberLit 2.0))
      sos :: SetOfSupport (LiteralF String) V (TermF String)
      sos = Set.fromList [(INF {neg = Set.fromList [(s58 (x))],
                                pos = Set.fromList [(s58 (f53 [x,n1 []]))]},
                           Map.fromList [(V "x",x)])]

      kb :: Set (WithId (ImplicativeForm (LiteralF String)))
      kb = Set.fromList [WithId {wiItem = INF {neg = Set.fromList [],
                                               pos = Set.fromList [(s51 (sk1 [x]) (x))]},
                                 wiIdent = 1},
                         WithId {wiItem = INF {neg = Set.fromList [],
                                               pos = Set.fromList [(s52
                                                                    (f53 [sk1 [x], n2 []]))]},
                                 wiIdent = 1},
                         WithId {wiItem = INF {neg = Set.fromList [],
                                               pos = Set.fromList [(s52 (sk1 [x]))]},
                                 wiIdent = 1},
                         WithId {wiItem = INF {neg = Set.fromList [],
                                               pos = Set.fromList [(s58 (n1 []))]},
                                 wiIdent = 4},
                         WithId {wiItem = INF {neg = Set.fromList [(s49 (x))],
                                               pos = Set.fromList [(s54 (x))]},
                                 wiIdent = 3},
                         WithId {wiItem = INF {neg = Set.fromList [(s49 (x))],
                                               pos = Set.fromList [(a155 (x))]},
                                 wiIdent = 2},
                         WithId {wiItem = INF {neg = Set.fromList [(a155 (x))],
                                               pos = Set.fromList [(s49 (x))]},
                                 wiIdent = 2}]

      expected :: (Bool, Set (ImplicativeForm (LiteralF String), Map.Map V (TermF String)))
      expected = (False,
                  fromList [(INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Function (NumberLit 1.0)) [],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []],fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))]),
                            (INF {neg = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (vt (V "x")))], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [vt (V "x"),fApp (Function (NumberLit 1.0)) []]))]},Map.fromList [(V "x",vt (V "x"))])])

prove2 :: Test
prove2 =
    TestCase (assertEqual "Prover bug 2" expected (prove limit Set.empty sos (Set.map wiItem kb)))
    where
      limit = Just 32
      sos :: SetOfSupport (LiteralF String) V (TermF String)
      sos = Set.fromList [(INF {neg = Set.fromList [],
                                pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 60)) (vt (V "x")) (vt (V "y")))]}, Map.fromList [(V "x",vt (V "x")),(V "y",vt (V "y"))]),
                          (INF {neg = Set.fromList [],
                                pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (fApp (Skolem "16" 1) [vt (V "x"),vt (V "y")]) (vt (V "x")))]}, Map.fromList [(V "x",vt (V "x")),(V "y",vt (V "y"))]),
                          (INF {neg = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 60)) (vt (V "x")) (vt (V "y"))),(pApp2 (Reference 2 (unsafeSubjectId 61)) (vt (V "z")) (vt (V "x")))],
                                pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (vt (V "z")) (vt (V "y")))]}, Map.fromList [(V "x",vt (V "x")),(V "y",vt (V "y")),(V "z",vt (V "z"))]),
                          (INF {neg = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (vt (V "z")) (vt (V "x")))],
                                pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (vt (V "z")) (vt (V "y")))]}, Map.fromList [(V "x",vt (V "x")),(V "y",vt (V "y")),(V "z",vt (V "z"))]),
                          (INF {neg = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (vt (V "z")) (vt (V "x")))],
                                pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (fApp (Skolem "16" 1) [vt (V "x"),vt (V "y")]) (vt (V "x")))]}, Map.fromList [(V "x",vt (V "x")),(V "y",vt (V "y")),(V "z",vt (V "z"))]),
                          (INF {neg = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (vt (V "z")) (vt (V "x"))),(pApp2 (Reference 2 (unsafeSubjectId 61)) (fApp (Skolem "16" 1) [vt (V "x"),vt (V "y")]) (vt (V "y")))],
                                pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (vt (V "z")) (vt (V "y")))]}, Map.fromList [(V "x",vt (V "x")),(V "y",vt (V "y")),(V "z",vt (V "z"))]),
                          (INF {neg = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (fApp (Skolem "16" 1) [vt (V "x"),vt (V "y")]) (vt (V "y")))],
                                pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 60)) (vt (V "x")) (vt (V "y")))]}, Map.fromList [(V "x",vt (V "x")),(V "y",vt (V "y"))])]
      kb :: Set (WithId (ImplicativeForm (LiteralF String)))
      kb = Set.fromList [WithId {wiItem = INF {neg = Set.fromList [], pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 51)) (fApp (Skolem "1" 1) [vt (V "x")]) (vt (V "x")))]}, wiIdent = 1},
                         WithId {wiItem = INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 52)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [fApp (Skolem "1" 1) [vt (V "x")],fApp (Function (NumberLit 2.0)) []]))]}, wiIdent = 1},
                         WithId {wiItem = INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 52)) (fApp (Skolem "1" 1) [vt (V "x")]))]}, wiIdent = 1},
                         WithId {wiItem = INF {neg = Set.fromList [], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (NumberLit 1.0)) []))]}, wiIdent = 4},
                         WithId {wiItem = INF {neg = Set.fromList [((vt (V "y")) .=. (fApp (Function (Reference 3 (unsafeSubjectId 53))) [vt (V "x"),fApp (Function (NumberLit 1.0)) []]))], pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 59)) (vt (V "y")) (vt (V "x")))]}, wiIdent = 6},
                         WithId {wiItem = INF {neg = Set.fromList [((fApp (Function (Reference 2 (unsafeSubjectId 59))) [vt (V "x")]) .=. (fApp (Function (Reference 2 (unsafeSubjectId 59))) [vt (V "y")]))], pos = Set.fromList [((vt (V "x")) .=. (vt (V "y")))]}, wiIdent = 8},
                         WithId {wiItem = INF {neg = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 49)) (vt (V "x")))], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 54)) (vt (V "x")))]}, wiIdent = 3},
                         WithId {wiItem = INF {neg = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 49)) (vt (V "x")))], pos = Set.fromList [(pApp1 (AssertionRef (unsafeAssertionId 155)) (vt (V "x")))]}, wiIdent = 2},
                         WithId {wiItem = INF {neg = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (vt (V "x")))], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (Reference 3 (unsafeSubjectId 53))) [vt (V "x"),fApp (Function (NumberLit 1.0)) []]))]}, wiIdent = 5},
                         WithId {wiItem = INF {neg = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 58)) (vt (V "x"))),(pApp2 (Reference 2 (unsafeSubjectId 59)) (fApp (Function (NumberLit 1.0)) []) (vt (V "x")))], pos = Set.fromList []}, wiIdent = 7},
                         WithId {wiItem = INF {neg = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 59)) (vt (V "y")) (vt (V "x")))], pos = Set.fromList [((vt (V "y")) .=. (fApp (Function (Reference 3 (unsafeSubjectId 53))) [vt (V "x"),fApp (Function (NumberLit 1.0)) []]))]}, wiIdent = 6},
                         WithId {wiItem = INF {neg = Set.fromList [(pApp1 (AssertionRef (unsafeAssertionId 155)) (vt (V "x")))], pos = Set.fromList [(pApp1 (Reference 1 (unsafeSubjectId 49)) (vt (V "x")))]}, wiIdent = 2}]

      expected :: (Bool, Set (ImplicativeForm (LiteralF String), Map.Map V (TermF String)))
      expected = (False, Set.fromList [(INF {neg = Set.fromList [],
                                              pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 60)) (vt (V "x")) (vt (V "y")))]},
                                         Map.fromList [(V "x",vt (V "x")),(V "y",vt (V "y"))]),
                                        (INF {neg = Set.fromList [],
                                              pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (fApp (Skolem "16" 1) [vt (V "x"),vt (V "y")]) (vt (V "x")))]},
                                         Map.fromList [(V "x",vt (V "x")),(V "y",vt (V "y"))]),
                                        (INF {neg = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 60)) (vt (V "x")) (vt (V "y"))),
                                                                  (pApp2 (Reference 2 (unsafeSubjectId 61)) (vt (V "z")) (vt (V "x")))],
                                              pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (vt (V "z")) (vt (V "y")))]},
                                         Map.fromList [(V "x",vt (V "x")),(V "y",vt (V "y")),(V "z",vt (V "z"))]),
                                        (INF {neg = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (vt (V "z")) (vt (V "x")))],
                                              pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (vt (V "z")) (vt (V "y")))]},
                                         Map.fromList [(V "x",vt (V "x")),(V "y",vt (V "y")),(V "z",vt (V "z"))]),
                                        (INF {neg = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (vt (V "z")) (vt (V "x")))],
                                              pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (fApp (Skolem "16" 1) [vt (V "x"),vt (V "y")]) (vt (V "x")))]},
                                         Map.fromList [(V "x",vt (V "x")),(V "y",vt (V "y")),(V "z",vt (V "z"))]),
                                        (INF {neg = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (vt (V "z")) (vt (V "x"))),
                                                                  (pApp2 (Reference 2 (unsafeSubjectId 61)) (fApp (Skolem "16" 1) [vt (V "x"),vt (V "y")]) (vt (V "y")))],
                                              pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (vt (V "z")) (vt (V "y")))]},
                                         Map.fromList [(V "x",vt (V "x")),(V "y",vt (V "y")),(V "z",vt (V "z"))]),
                                        (INF {neg = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 61)) (fApp (Skolem "16" 1) [vt (V "x"),vt (V "y")]) (vt (V "y")))],
                                              pos = Set.fromList [(pApp2 (Reference 2 (unsafeSubjectId 60)) (vt (V "x")) (vt (V "y")))]},
                                         Map.fromList [(V "x",vt (V "x")),(V "y",vt (V "y"))])])


atomic2 :: Test
atomic2 =
    TestCase (assertEqual "Atom test 2" expected input)
    where
      input :: Set (ImplicativeForm LiteralPF)
      input = runSkolem (implicativeNormalForm (pApp (Reference 1 (unsafeSubjectId 58) :: AtomicPredicate Description) [fApp (Function (NumberLit 1.0) :: AtomicFunction Description V) []] :: FormulaPF) :: SkolemM (FunOf (TermOf (AtomOf LiteralPF))) (Set (ImplicativeForm LiteralPF)))
      expected :: Set (ImplicativeForm LiteralPF)
      expected = Set.fromList [INF {neg = Set.fromList [],
                                    pos = Set.fromList [atomic (N.Apply (Reference 1 (unsafeSubjectId 58)) [N.FunApp (Function (NumberLit 1.0)) []])]}]

atomic3 :: Test
atomic3 =
    TestCase (assertEqual "Atom test 3" expected input)
    where
      input = compare f0 f1
      f0 = runSkolem (skolemize id (pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (NumberLit 0.0)) []) :: FormulaPF)) :: PFormula (AtomOf FormulaPF)
      f1 = runSkolem (skolemize id (pApp1 (Reference 1 (unsafeSubjectId 58)) (fApp (Function (NumberLit 1.0)) []) :: FormulaPF)) :: PFormula (AtomOf FormulaPF)
      expected = LT

atomic4 :: Test
atomic4 =
    TestCase (assertEqual "Atom test 4" expected input)
    where
      input = compare f0 f1
      f0 = markPublic (N.Predicate (N.Apply (Reference 1 (unsafeSubjectId 58)) [N.FunApp (Function (NumberLit 0.0)) []])) :: FormulaPF
      f1 = markPublic (N.Predicate (N.Apply (Reference 1 (unsafeSubjectId 58)) [N.FunApp (Function (NumberLit 1.0)) []]))
      expected = LT

atomic5 :: Test
atomic5 =
    TestCase (assertEqual "Atom test 5" expected input)
    where
      input = compare f0 f1
      f0 = N.Predicate (N.Apply (Reference 1 (unsafeSubjectId 58)) [N.FunApp (Function (NumberLit 0.0)) []]) :: N.NFormula V (AtomicPredicate Description) (AtomicFunction Description V)
      f1 = N.Predicate (N.Apply (Reference 1 (unsafeSubjectId 58)) [N.FunApp (Function (NumberLit 1.0)) []]) :: N.NFormula V (AtomicPredicate Description) (AtomicFunction Description V)
      -- f0 = N.FunApp (Function (NumberLit 0.0)) [] :: N.PTerm V (AtomicFunction Description)
      -- f1 = N.FunApp (Function (NumberLit 1.0)) [] :: N.PTerm V (AtomicFunction Description)
      -- f0 = Function (NumberLit 0.0) :: AtomicFunction Description
      -- f1 = Function (NumberLit 1.0) :: AtomicFunction Description
      -- f0 = NumberLit 0.0 :: AtomicPredicate Description
      -- f1 = NumberLit 1.0 :: AtomicPredicate Description
      expected = LT
