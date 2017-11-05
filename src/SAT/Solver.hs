{-# LANGUAGE TypeFamilies, FlexibleContexts, KindSignatures, AllowAmbiguousTypes #-}
module SAT.Solver where

import Data.Proxy ( Proxy )
import qualified Data.Set as Set
import Data.Either ( rights, lefts )

import Control.Monad.Trans.State.Lazy ( StateT )

import SAT.Types ( ESolution )
import SAT.Variables ( Var, VarCache, emptyCache )


type SolverAction s v t = StateT ((SolverState s) v) IO t

class Solver (s :: * -> *) where
    type SolverState s :: * -> *
    newSolver :: Proxy s -> IO ((SolverState s) v)
    solve :: SolverAction s v (ESolution v)

class (Ord (VariableType c)) => HasVariables c where
    {-# MINIMAL allVariables #-}
    -- | Defines what type variables of @c@ have
    type VariableType c

    -- | Extracts all variables (helper or not) from @c@.
    -- new helpers may be constructed using the @VarCache@.
    -- if @c@ has multiple occurances of a variable it has to be included multiple times in the result.
    allVariables :: c -> VarCache (VariableType c) -> [Var (VariableType c)]
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
