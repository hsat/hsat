{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module TestUtils where

import Data.Int

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty.HUnit

import SAT.Types
    

instance Monad m => Serial m Word where
    series = cons1 (toEnum . getPositive)
instance Monad m => Serial m Int8 where
    series = cons1 toEnum
instance Serial m a => Serial m (Lit a)