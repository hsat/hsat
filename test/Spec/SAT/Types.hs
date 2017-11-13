{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Spec.SAT.Types ( tests ) where

import GHC.Generics ( Generic )
import GHC.Exts ( IsList ( Item, toList, fromList ) )

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Vector ( Vector )

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty.HUnit
import TestUtils

import SAT.Types

instance Serial m a => Serial m (Vector a) where
    series = fromList <$> series
instance Monad m => Serial m LBool where
instance Serial m a => Serial m (Solution a) where

newtype B = B Bool deriving (Generic, Show, Ord, Eq)
instance Monad m => Serial m B where

tests = testGroup "Types" 
    [ testGroup "Solution"
        [ testCase "solutionAsMap" $
            solutionAsMap (Solution (fromList [("a", LTrue), ("b", LFalse)])) @?=
                Map.fromList [("a", LTrue), ("b", LFalse)]
        , testCase "IsList" $
            Solution (fromList [("a", LTrue), ("b", LFalse)]) @?=
                [("a", LTrue), ("b", LFalse)]
        , testGroup "Applicative"
            [ testProperty "identity"      solutionApplicativeIdentity
            , testProperty "composition" $ changeDepth half solutionApplicativeComposition
            , testProperty "homomorphism"  solutionApplicativeHomomorphism
            , testProperty "interchange"   solutionApplicativeInterchange
            ]
        ]
    ]

pureSol :: a -> Solution a
pureSol = pure

solutionApplicativeIdentity :: Solution Bool -> Res
solutionApplicativeIdentity v = (pure id <*> v) === v

solutionApplicativeComposition :: Solution (Bool -> Bool) -> Solution (Bool -> Bool) -> Solution Bool -> Res
solutionApplicativeComposition u v w = (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))

solutionApplicativeHomomorphism :: (Bool -> Bool) -> Bool -> Res
solutionApplicativeHomomorphism f x = (pure f <*> pure x) === pureSol (f x)

solutionApplicativeInterchange :: Solution (Bool -> Bool) -> Bool -> Res
solutionApplicativeInterchange u y = (u <*> pure y) === (pureSol ($ y) <*> u)