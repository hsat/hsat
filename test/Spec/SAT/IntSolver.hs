module Spec.SAT.IntSolver ( tests ) where


import Control.Monad.Trans.State.Lazy ( StateT )
import Control.Monad.Trans.Class ( lift )

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty.HUnit
import TestUtils

import SAT.IntSolver
import SAT.Types
import SAT.Util
import SAT.PicoSAT

tests = testGroup "IntSolver" []

main = evalIntSolver picoSAT $ do
    add [ 1, 2]
    add [-1, 2]
    printSolve
  where
    add :: [Lit Word] -> StateT PicoSAT IO ()
    add = addIntClause
    printSolve = lift . putStr . prettyESolution =<< solve
