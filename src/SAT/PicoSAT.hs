-- | Provides a binding to PicoSAT.
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module SAT.PicoSAT
    ( IntPicoSAT
    , intPicoSAT
    ) where

import GHC.Generics ( Generic )

import Data.Proxy (Proxy(Proxy))

import Control.Monad.Trans.State.Lazy ( get )

import SAT.IntSolver
import SAT.IntSolver.IPASIR

import Foreign.ForeignPtr ( ForeignPtr )
import Foreign.C.Types ( CInt( CInt ) )


foreign import ccall unsafe "ipasir.h  ipasir_signature" ipasir_signature :: IpasirSignature
foreign import ccall unsafe "ipasir.h  ipasir_init"      ipasir_init      :: IpasirInit
foreign import ccall unsafe "ipasir.h &ipasir_release"   ipasir_release   :: IpasirRelease
foreign import ccall unsafe "ipasir.h  ipasir_add"       ipasir_add       :: IpasirAdd
foreign import ccall unsafe "ipasir.h  ipasir_assume"    ipasir_assume    :: IpasirAssume
foreign import ccall unsafe "ipasir.h  ipasir_solve"     ipasir_solve     :: IpasirSolve
foreign import ccall unsafe "ipasir.h  ipasir_val"       ipasir_val       :: IpasirVal
foreign import ccall unsafe "ipasir.h  ipasir_failed"    ipasir_failed    :: IpasirFailed

-- | A PicoSAT solver operating on Ints.
data IntPicoSAT = IntPicoSAT Word (ForeignPtr ())
    deriving (Eq, Ord, Generic)

-- | @Proxy@ to @IntPicoSAT@ for ease of use.
intPicoSAT :: Proxy IntPicoSAT
intPicoSAT = Proxy

instance IpasirSolver IntPicoSAT where
instance Show IntPicoSAT where
    show = ipasirShow ipasir_signature
instance IntSolver IntPicoSAT where
    newIntSolver _ = ipasirNewIntSolver ipasir_init ipasir_release
    addIntClause = ipasirAddIntClause ipasir_add
    addIntAssumption = ipasirAddIntAssumption ipasir_assume
    numIntVars = nVars <$> get
    solveInt = ipasirSolve ipasir_solve ipasir_val ipasir_failed
