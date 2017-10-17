{-# LANGUAGE TemplateHaskell #-}
module SAT.PicoSAT
    ( picoSAT
    , PicoSAT
    ) where

import SAT.IntSolver.IPASIR.TH

import Foreign.Ptr ( Ptr )
import Foreign.C.Types ( CInt( CInt ), CChar ( CChar ) )

foreign import ccall unsafe "ipasir.h  ipasir_signature" ipasir_signature :: IpasirSignature
foreign import ccall unsafe "ipasir.h  ipasir_init"      ipasir_init      :: IpasirInit
foreign import ccall unsafe "ipasir.h &ipasir_release"   ipasir_release   :: IpasirRelease
foreign import ccall unsafe "ipasir.h  ipasir_add"       ipasir_add       :: IpasirAdd
foreign import ccall unsafe "ipasir.h  ipasir_assume"    ipasir_assume    :: IpasirAssume
foreign import ccall unsafe "ipasir.h  ipasir_solve"     ipasir_solve     :: IpasirSolve
foreign import ccall unsafe "ipasir.h  ipasir_val"       ipasir_val       :: IpasirVal
foreign import ccall unsafe "ipasir.h  ipasir_failed"    ipasir_failed    :: IpasirFailed

$(ipasirSolver "PicoSAT"
    [e| ipasir_signature |]
    [e| ipasir_init      |]
    [e| ipasir_release   |]
    [e| ipasir_add       |]
    [e| ipasir_assume    |]
    [e| ipasir_solve     |]
    [e| ipasir_val       |]
    [e| ipasir_failed    |])
