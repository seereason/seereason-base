#!/usr/bin/runhaskell

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import Distribution.Simple.Program
import System.Cmd
import System.Exit

main = defaultMainWithHooks simpleUserHooks {
         postBuild = \ _ _ _ lbi -> runTestScript lbi
       , runTests = \ _ _ _ lbi -> runTestScript lbi
       }

runTestScript lbi =
    system (buildDir lbi ++ "/tests/tests") >>= \ code ->
    if code == ExitSuccess then return () else error "Test Failure"
