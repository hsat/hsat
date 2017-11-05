-- | Contains typeclasses and utility functions for SAT-Solvers that operate on integer Literals.
-- To be able to use such a solver you have to choose a concrete implementation.
-- This package contains bindings for PicoSAT through 'SAT.PicoSAT'.
module SAT.IntSolver
    ( module SAT.IntSolver.Base
    , module SAT.IntSolver.Util
    ) where

import SAT.IntSolver.Base
import SAT.IntSolver.Util