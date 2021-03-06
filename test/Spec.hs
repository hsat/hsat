module Main where

import Test.Tasty

import Spec.SAT.Types
import Spec.SAT.Types.LBool
import Spec.SAT.Types.Lit
import Spec.SAT.IntSolver


main = defaultMain $ testGroup "Tests"
    [ Spec.SAT.Types.tests
    , Spec.SAT.Types.LBool.tests
    , Spec.SAT.Types.Lit.tests
    , Spec.SAT.IntSolver.tests
    ]
