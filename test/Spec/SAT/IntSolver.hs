module Spec.SAT.IntSolver where

import Control.Monad.Trans.Class
    ( lift
    )

import SAT.IntSolver
import SAT.Types
import Spec.SAT.IntSolver.IPASIR.TH


main = evalIntSolver cryptoMiniSat $ do
    addIntClause ([1, -2, 3] :: [Int])
    lift . print =<< solve
