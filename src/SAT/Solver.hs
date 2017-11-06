-- | Provides a common interface for incremental SAT solvers that accept custom representations of variables and clauses.
{-# LANGUAGE TypeFamilies, FlexibleContexts, KindSignatures, AllowAmbiguousTypes #-}
module SAT.Solver where

import Data.Proxy ( Proxy )
import qualified Data.Set as Set
import Data.Either ( rights, lefts )

import Control.Monad.Trans.State.Lazy ( StateT )

import SAT.Types ( ESolution )
import SAT.Variables ( Var, VarCache, emptyCache )

-- | An action performed by a @Solver@.
type SolverAction s t = StateT s IO t

-- | A SAT solver.
class Solver (s :: * -> *) where

    -- | create a new, empty solver.
    newSolver :: Proxy s -> IO (s v)
    -- | find a solution for alle added clauses and assumptions.
    -- This will clear all previous assumptions.
    solve :: SolverAction (s v) (ESolution v)

-- | Something that provides contains.
class (Ord (VariableType c)) => HasVariables c where
    {-# MINIMAL allVariables | (allLabels, allHelpers) #-}
    -- | Defines what type variables of @c@ have
    type VariableType c

    -- | Extracts all variables (helper or not) from @c@.
    -- new helpers may be constructed using the @VarCache@.
    -- if @c@ has multiple occurances of a variable it has to be included multiple times in the result.
    allVariables :: c -> VarCache (VariableType c) -> [Var (VariableType c)]
    allVariables c vc = (Right <$> allLabels c) ++ (Left <$> allHelpers c vc)
    -- | Extract all labels (variables that aren't helpers) from @c@
    -- if @c@ has multiple occurances of a variable it has to be included multiple times in the result.
    allLabels :: c -> [VariableType c]
    allLabels c = rights $ allVariables c emptyCache
    -- | Extract all helpers from @c@.
    -- new helpers have to be created using the @VarCache@
    -- if @c@ has multiple occurances of a variable it has to be included multiple times in the result.
    allHelpers :: c -> VarCache (VariableType c) -> [Word]
    allHelpers c vc = lefts $ allVariables c vc
    
    -- | Extracts all variables (helper or not) from @c@.
    -- new helpers may be constructed using the @VarCache@.
    variables :: c -> VarCache (VariableType c) -> Set.Set (Var (VariableType c))
    variables c vc = Set.fromList $ allVariables c vc
    -- | Extract all labels (variables that aren't helpers) from @c@
    labels :: c -> Set.Set (VariableType c)
    labels c = Set.fromList $ allLabels c
    -- | Extract all helpers from @c@.
    -- new helpers have to be created using the @VarCache@
    helpers :: c -> VarCache (VariableType c) -> Set.Set Word
    helpers c vc = Set.fromList $ allHelpers c vc
