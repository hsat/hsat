{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Spec.SAT.Types.LBool ( tests ) where

import Data.Monoid
import Data.Function ( on )

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty.HUnit

import SAT.Types
import TestUtils

instance Monad m => Serial m LBool
instance Monad m => Serial m LAll
instance Monad m => Serial m LAny

tests = testGroup "LBool"
    [ testGroup "Eq"
        [ testProperty "((.).(.)) not (/=) = (==)" $ ((.).(.)) not (/=) =..= ((==) :: LBool -> LBool -> Bool)
        , testProperty "(==) `on` toEnum = (==)" $ ((==) `on` fromEnum) =..= ((==) :: LBool -> LBool -> Bool)
        , testProperty "(==) `on` LAll   = (==)" $ ((==) `on` LAll    ) =..= ((==) :: LBool -> LBool -> Bool)
        , testProperty "(==) `on` LAny   = (==)" $ ((==) `on` LAny    ) =..= ((==) :: LBool -> LBool -> Bool)
        , testProperty "(==) `on` shrink = (==)" $ ((==) `on` (shrink :: LBool -> Maybe Bool)) =..= ((==) :: LBool -> LBool -> Bool)
        ]  
    , testGroup "Ord"
        [ testProperty "compare `on` toEnum = compare" $ (compare `on` fromEnum) =..= (compare :: LBool -> LBool -> Ordering)
        , testProperty "compare `on` LAll = compare"   $ (compare `on` LAll    ) =..= (compare :: LBool -> LBool -> Ordering)
        , testProperty "compare `on` LAny = compare"   $ (compare `on` LAny    ) =..= (compare :: LBool -> LBool -> Ordering)
        ]
    , testGroup "Enum"
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
    , testGroup "LAll"
        [ testCase "LTrue = getLAll mempty" $ LTrue @?= getLAll mempty
        , testGroup "Monoid"
            [ testProperty "identity" $ \ x -> x <> mempty === (x :: LAll)
            , testProperty "identity reverse" $ \ x -> mempty <> x === (x :: LAll)
            , testProperty "associativity" $ \ x y z -> x <> (y <> z) === (x <> y) <> (z :: LAll)
            ]
        ]
    , testGroup "LAny"
        [ testCase "LFalse = getLAny mempty" $ LFalse @?= getLAny mempty
        , testGroup "Monoid"
            [ testProperty "identity" $ \ x -> x <> mempty === (x :: LAny)
            , testProperty "identity reverse" $ \ x -> mempty <> x === (x :: LAny)
            , testProperty "associativity" $ \ x y z -> x <> (y <> z) === (x <> y) <> (z :: LAny)
            ]
        ]
    ]

enumTransitive :: LBool -> Bool
enumTransitive lbool = lbool == lbool

propBoolToLBool :: Bool -> Bool
propBoolToLBool b = [b] == shrink (boolToLBool b)