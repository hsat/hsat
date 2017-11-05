module SAT.IntSolver.Util
    ( unfoldSolveInt
    ) where

import Control.Monad.Trans.State.Lazy ( StateT )

import Data.Monoid ( Monoid ( mempty ) )

import SAT.IntSolver.Base ( IntSolver(..) )
import SAT.Types ( Solution, SomeSolutions(..), ESolution(..) )


unfoldSolveInt :: IntSolver s
               => (Solution Word -> b -> StateT s IO (Maybe b))
               -> b
               -> StateT s IO (SomeSolutions Word)
unfoldSolveInt f b0 = do
    esol <- solveInt
    case esol of
        EConflict conflict -> return $ SomeSolutions (Just conflict) mempty
        ESolution sol -> maybe ( return $ SomeSolutions Nothing mempty ) (nextUnfold sol) =<< f sol b0
  where
    nextUnfold sol b = do
        (SomeSolutions conflict sols) <- unfoldSolveInt f b
        return $ SomeSolutions conflict (sol : sols)
        