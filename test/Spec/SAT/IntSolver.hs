{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Spec.SAT.IntSolver ( tests ) where

import Control.Monad.Trans.State.Lazy ( StateT )
import Control.Monad.Trans.Class ( lift )
import Control.Comonad ( extract )
import System.IO.Unsafe ( unsafePerformIO )

import qualified Data.Map as Map
import Data.Maybe ( isJust )
import Data.List ( find )

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty.HUnit
import TestUtils

import SAT.IntSolver
import SAT.Types
import SAT.Util
import SAT.PicoSAT


tests = testGroup "IntSolver"
    [ testProperty "solves corretly" $ monadic . propSolve
    ]

newtype Cnf = Cnf [[Lit Word]] deriving (Show, Eq)
instance Monad m => Serial m Cnf where
    series = Cnf <$> localDepth succ series

propSolve :: Cnf -> IO (Either Reason Reason)
propSolve (Cnf cnf) = evalIntSolver intPicoSAT $ do
    addIntClauses cnf
    res <- intSolve
    return $ case res of
        (ESolution sol) -> if testSolution sol cnf
            then Right "OK"
            else Left $ prettySolution sol
        (EConflict conf) -> Right $ prettyConflict conf

testSolution sol = all $ any val
  where
    val = isJust . find (\l -> isLitPositive l == extract l ) . (traverse shrink :: Lit LBool -> [Lit Bool]) . fmap (solutionAsMap sol Map.!)