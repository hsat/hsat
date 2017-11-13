{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module TestUtils where

import Data.Int
import Data.Function ( on )

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty.HUnit

import SAT.Types


half :: Depth -> Depth
half = (`div` 2)

type Res = Either String String

(=..=) :: (Show c, Eq c) => (a -> b -> c) -> (a -> b -> c) -> a -> b -> Res
f =..= g = \ a -> f a =.= g a
infix 4 =..=

(=.=) :: (Show b, Eq b) => (a -> b) -> (a -> b) -> a -> Res
f =.= g = \ a -> f a === g a
infix 4 =.=

(===) :: (Show a, Eq a) => a -> a -> Res
a === b
    | a == b    = Right $ show a ++ " == " ++ show b
    | otherwise = Left  $ show a ++ " /= " ++ show b
infix 4 ===
    

instance Monad m => Serial m Word where
    series = cons1 (toEnum . getPositive)
instance Monad m => Serial m Int8 where
    series = cons1 toEnum
instance Serial m a => Serial m (Lit a)
