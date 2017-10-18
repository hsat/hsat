{-# LANGUAGE FlexibleContexts #-}
module SAT.IntSolver
    ( IntSolver(..)
    , evalIntSolver
    ) where

import Control.Monad.Trans.State.Lazy
    ( StateT
    , evalStateT
    )

import Data.Foldable
    ( traverse_
    )

import Data.Proxy
    ( Proxy
    )

import SAT.Types
    ( ESolution
    , Lit
    , IsLit
    )


evalIntSolver :: IntSolver s => Proxy s -> StateT s IO a -> IO a
evalIntSolver solverProxy action = evalStateT action =<< newIntSolver solverProxy

class IntSolver s where
    newIntSolver :: Proxy s -> IO s
    addIntClause :: (Foldable f) => f (Lit Word) -> StateT s IO ()
    addIntClauses :: (Foldable f, Foldable g) => f (g (Lit Word)) -> StateT s IO ()
    addIntClauses = traverse_ addIntClause
    numVars :: StateT s IO Word
    solve :: StateT s IO (ESolution Word)
