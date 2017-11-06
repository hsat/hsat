-- | Provides utility functions ontop of @IntSolver@.
-- For example ways to extract all solutions for a given clause.
module SAT.IntSolver.Util
    ( unfoldSolveInt
    ) where

import Data.Monoid ( Monoid ( mempty ) )

import SAT.IntSolver.Base ( IntSolver(..), IntSolverAction )
import SAT.Types ( Solution, SomeSolutions(..), ESolution(..) )


-- | Performs the provided solver action for each solution as long as it does not return @Nothing@
-- and returns all solutions produced on the way.
-- 
-- The main use of this function is to serve as base for all other standalone solving functions provided by this module.
unfoldSolveInt :: IntSolver s
               => (Solution Word -> b -> IntSolverAction s (Maybe b))
               -> b
               -> IntSolverAction s (SomeSolutions Word)
unfoldSolveInt f b0 = do
    esol <- solveInt
    case esol of
        EConflict conflict -> return $ SomeSolutions (Just conflict) mempty
        ESolution sol -> maybe ( return $ SomeSolutions Nothing mempty ) (nextUnfold sol) =<< f sol b0
  where
    nextUnfold sol b = do
        (SomeSolutions conflict sols) <- unfoldSolveInt f b
        return $ SomeSolutions conflict (sol : sols)
        