-- | Generic implementation of @IpasirSolver@.
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module SAT.IntSolver.IPASIR.Generic where

import GHC.Generics

import Control.Applicative ( Alternative ( (<|>) ) )

import Foreign.ForeignPtr ( ForeignPtr )


-- | Generic version of @IpasirSolver@.
class GIpasirSolver (s :: * -> *) where
    
    -- | Generic version of @nVars@
    gNVars :: s a -> Maybe Word
    -- | Generic version of @ptr@
    gPtr :: s a -> Maybe (ForeignPtr ())
    -- | Generic version of @ctr@
    gCtr :: Word -> ForeignPtr () -> s a

-- | Strips metadata.
instance (GIpasirSolver f) => GIpasirSolver (M1 i c f) where
    gNVars = gNVars . unM1
    gPtr = gPtr . unM1
    gCtr n p = M1 $ gCtr n p

-- | Product takes the first nonempty for @gNVars@ and @gPtr@ and composes the constructors.
instance (GIpasirSolver a, GIpasirSolver b) => GIpasirSolver (a :*: b) where
    gNVars (a :*: b) = gNVars a <|> gNVars b
    gPtr   (a :*: b) = gPtr a   <|> gPtr b
    gCtr n p = gCtr n p :*: gCtr n p

-- | Extract nVars
instance GIpasirSolver (K1 i Word) where
    gNVars = Just . unK1
    gPtr _ = Nothing
    gCtr n _ = K1 n

-- | Extract ptr
instance GIpasirSolver (K1 i (ForeignPtr ())) where
    gNVars _ = Nothing
    gPtr =  Just . unK1
    gCtr _ = K1