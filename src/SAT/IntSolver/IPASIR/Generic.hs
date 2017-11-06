{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module SAT.IntSolver.IPASIR.Generic where

import GHC.Generics

import Control.Applicative ( Alternative ( (<|>) ) )

import Foreign.ForeignPtr ( ForeignPtr )


class GIpasirSolver (s :: * -> *) where
    gNVars :: s a -> Maybe Word
    gPtr :: s a -> Maybe (ForeignPtr ())
    gCtr :: Word -> ForeignPtr () -> s a

instance (GIpasirSolver f) => GIpasirSolver (M1 i c f) where
    gNVars = gNVars . unM1
    gPtr = gPtr . unM1
    gCtr n p = M1 $ gCtr n p

instance (GIpasirSolver a, GIpasirSolver b) => GIpasirSolver (a :*: b) where
    gNVars (a :*: b) = gNVars a <|> gNVars b
    gPtr   (a :*: b) = gPtr a   <|> gPtr b
    gCtr n p = gCtr n p :*: gCtr n p

instance GIpasirSolver (K1 i Word) where
    gNVars = Just . unK1
    gPtr _ = Nothing
    gCtr n _ = K1 n

instance GIpasirSolver (K1 i (ForeignPtr ())) where
    gNVars _ = Nothing
    gPtr =  Just . unK1
    gCtr _ = K1