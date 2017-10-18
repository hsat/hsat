{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Spec.SAT.Types.LBool ( tests ) where

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty.HUnit

import SAT.Types


instance Monad m => Serial m LBool

tests = testGroup "### LBool"
    [ testGroup "Enum"
        [ testProperty "transitive" enumTransitive
        , testCase "fromEnum" $ do
            assertEqual " 1 = fromEnum LTrue " ( 1) (fromEnum LTrue)
            assertEqual "-1 = fromEnum LFalse" (-1) (fromEnum LFalse)
            assertEqual " 0 = fromEnum LUndef" ( 0) (fromEnum LUndef)
        , testCase "toEnum" $ do
            assertEqual "LTrue  = toEnum  1" LTrue  $ toEnum ( 1)
            assertEqual "LFalse = toEnum -1" LFalse $ toEnum (-1)
            assertEqual "LUndef = toEnum  0" LUndef $ toEnum ( 0)
        ]
    , testCase "explode" $ do
        assertEqual "explode LTrue  = [True]"        [True]        $ explode LTrue
        assertEqual "explode LFalse = [False]"       [False]       $ explode LFalse
        assertEqual "explode LUndef = [True, False]" [True, False] $ explode LUndef
    , testCase "shrink" $ do
        assertEqual "shrink LTrue  = [True] " [True]  $ shrink LTrue
        assertEqual "shrink LFalse = [False]" [False] $ shrink LFalse
        assertEqual "shrink LUndef = []     " []      $ shrink LUndef
    , testCase "maybeToLBool" $ do
        assertEqual "Just True  = maybeToLBool LTrue " LTrue  $ maybeToLBool $ Just True
        assertEqual "Just False = maybeToLBool LFalse" LFalse $ maybeToLBool $ Just False
        assertEqual "Nothing    = maybeToLBool LUndef" LUndef $ maybeToLBool $ Nothing
    , testCase "lBoolToChar" $ do
        assertEqual "'+' = lBoolToChar LTrue " '+' $ lBoolToChar LTrue
        assertEqual "'-' = lBoolToChar LFalse" '-' $ lBoolToChar LFalse 
        assertEqual "'?' = lBoolToChar LUndef" '?' $ lBoolToChar LUndef
    , testProperty "boolToLBool" propBoolToLBool
    ]

enumTransitive :: LBool -> Bool
enumTransitive lbool = lbool == lbool

propBoolToLBool :: Bool -> Bool
propBoolToLBool b = [b] == shrink (boolToLBool b)