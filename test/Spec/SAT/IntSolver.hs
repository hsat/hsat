module Spec.SAT.IntSolver where


import Control.Monad.Trans.State.Lazy ( StateT )
import Control.Monad.Trans.Class ( lift )

import SAT.IntSolver
import SAT.Types
import SAT.Util
import Spec.SAT.IntSolver.IPASIR.TH


main = evalIntSolver cryptoMiniSat $ do
    add [ 1, 2]
    add [-1, 2]
    printSolve
    printSolve
  where
    add :: [Int] -> StateT CryptoMiniSat IO ()
    add = addIntClause
    printSolve = lift . putStr . prettyESolution =<< solve
