module Spec.SAT.IntSolver where


import Control.Monad.Trans.State.Lazy ( StateT )
import Control.Monad.Trans.Class ( lift )

import SAT.IntSolver
import SAT.Types
import SAT.Util
import SAT.PicoSAT


main = evalIntSolver picoSAT $ do
    add [ 1, 2]
    add [-1, 2]
    printSolve
  where
    add :: [Int] -> StateT PicoSAT IO ()
    add = addIntClause
    printSolve = lift . putStr . prettyESolution =<< solve
