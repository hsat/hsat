module Main where

import Test.Tasty

import Spec.SAT.Types.LBool
import Spec.SAT.Types.Lit


main = defaultMain Main.tests

tests = testGroup "Tests"
    [ Spec.SAT.Types.LBool.tests
    , Spec.SAT.Types.Lit.tests
    ]
