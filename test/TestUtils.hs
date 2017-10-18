{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module TestUtils where

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty.HUnit
    

instance Monad m => Serial m Word where
    series = cons1 (toEnum . getPositive)