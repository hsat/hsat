-- | Base module für 'IntSolver'.
-- This provides only the @IntSolver@ class itself, the @evalIntSolver@ function and the @IntSolverAction@ type.
-- Anything ontop of these definitions is defined in 'SAT.InstSolver.Util' and exposed through 'SAT.InstSolver'.
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module SAT.IntSolver.Base
    ( IntSolver(..)
    , evalIntSolver
    , IntSolverAction
    ) where

import Data.Proxy ( Proxy )

import Control.Monad.Trans.State.Lazy ( StateT, evalStateT )
import Data.Foldable ( traverse_ )

import SAT.Types ( ESolution, Lit )


-- | evaluates a solver computation with the given solver and return the final value.
evalIntSolver :: IntSolver s
              => Proxy s             -- ^ concrete solver implementation to use
              -> IntSolverAction s a -- ^ solver computation
              -> IO a
evalIntSolver solverProxy action = evalStateT action =<< newIntSolver solverProxy

-- | An action perfomed by an @IntSolver@.
type IntSolverAction s t = StateT s IO t

-- | An IntSolver is a SAT-Solver that represents variables as positive, natural numbers. 
class IntSolver s where

    -- | create a new instance of the solver.
    newIntSolver :: Proxy s -> IO s

    -- | add the given cnf clause to the solver.
    -- former clauses are not discarded.
    addIntClause :: (Foldable f) => f (Lit Word) -> IntSolverAction s ()

    -- | add all given cnf clauses to the solver.
    -- former clauses are not discarded.
    addIntClauses :: (Foldable f, Foldable g) => f (g (Lit Word)) -> IntSolverAction s ()
    addIntClauses = traverse_ addIntClause

    -- | add an assumption to the solver.
    -- This assumption will only be used for the next solution.
    -- Assumptions added since the last invocation of 'solveInt' are not discarded.
    -- 
    -- A Solver does not have to implement this function in a meaningful way
    addIntAssumption :: Lit Word -> IntSolverAction s ()
    addIntAssumption _ = return ()
    
    -- | add all assumptions to the solver.
    -- These assumptions will only be used for the next solution.
    -- Assumptions added since the last invocation of 'solveInt' are not discarded.
    -- 
    -- A Solver does not have to implement this function in a meaningful way
    addIntAssumptions :: (Foldable f) => f (Lit Word) -> IntSolverAction s ()
    addIntAssumptions = traverse_ addIntAssumption

    -- | Return the number of variables in all currently added clauses.
    -- This value may not reflect the actual number, but rather be the biggest
    -- number representing a variable.
    numIntVars :: IntSolverAction s Word

    -- | Try to find a solution for all added cnf clauses and currently active
    -- assumptions.
    -- This will clear all added assumptions.
    solveInt :: IntSolverAction s (ESolution Word)
    