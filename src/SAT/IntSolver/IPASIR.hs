-- | This module provides helper functions for ipasir.h any backed 'IntSolver'.
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module SAT.IntSolver.IPASIR
    ( ipasirNewIntSolver
    , ipasirShow
    , ipasirAddIntClause
    , ipasirAddIntAssumption
    , ipasirSolve

    , IpasirSolver(..)
    , IpasirSignature
    , IpasirInit
    , IpasirRelease
    , IpasirAdd
    , IpasirAssume
    , IpasirSolve
    , IpasirVal
    , IpasirFailed
    ) where

import GHC.Generics ( Generic ( from, to) , Rep )

import Data.Maybe ( fromJust )
import qualified Data.Vector as Vec

import Data.Foldable ( mapM_, toList )
import Control.Comonad ( extract )
import Control.Monad.Trans.State.Lazy ( get, put )
import Control.Monad.Trans.Class ( lift )

import System.IO.Unsafe ( unsafePerformIO )

import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr, withForeignPtr )
import Foreign.C.String ( CString, peekCString )
import Foreign.C.Types ( CInt )

import SAT.IntSolver.IPASIR.Generic
import SAT.IntSolver.Base ( IntSolverAction )
import SAT.Types
    ( ESolution(..), Solution(..), Conflict(..)
    , LBool (..), Lit, IsLit ( fromLit )
    )


-- | Signature for 'ipasir_signature' ffi functions.
type IpasirSignature = IO CString
-- | Signature for 'ipasir_init' ffi functions.
type IpasirInit = IO (Ptr ())
-- | Signature for 'ipasir_release' ffi function pointer.
type IpasirRelease = FunPtr (Ptr() -> IO ())
-- | Signature for 'ipasir_add' ffi function.
type IpasirAdd = Ptr () -> CInt -> IO ()
-- | Signature for 'ipasir_assume' ffi function.
type IpasirAssume = Ptr () -> CInt -> IO ()
-- | Signature for 'ipasir_solve' ffi function.
type IpasirSolve = Ptr () -> IO CInt
-- | Signature for 'ipasir_val' ffi function.
type IpasirVal = Ptr () -> CInt -> IO CInt
-- | Signature for 'ipasir_failed' ffi function.
type IpasirFailed = Ptr () -> CInt -> IO CInt


-- | Helper class for ipasir functions. Any Solver that uses these implementations has to have a way of
-- providing a pointer to the C-solver and keep track of the number of variables.
--
-- If the solver type is an adt of the form:
-- 
-- @
--     data MySolver = MySolver Word (ForeignPtr ())
-- @
-- 
-- You can derive @Generic@ and the @IpasirSolver@ instance will to the right thing.
class IpasirSolver s where

    -- | get the number of variables from solver.
    nVars :: s -> Word
    default nVars :: (Generic s, GIpasirSolver (Rep s)) => s -> Word
    nVars = fromJust . gNVars . from

    -- | get the pointer to the underlying C-solver.
    ptr :: s -> ForeignPtr ()
    default ptr :: (Generic s, GIpasirSolver (Rep s)) => s -> ForeignPtr ()
    ptr = fromJust . gPtr . from

    -- | create a new solver from the number of variables and pointer.
    ctr :: Word -> ForeignPtr () -> s
    default ctr :: (Generic s, GIpasirSolver (Rep s)) => Word -> ForeignPtr () -> s
    ctr n p = to $ gCtr n p

instance IpasirSolver (Word, ForeignPtr ()) where
    nVars = fst
    ptr   = snd
    ctr   = (,)
    

-- | 'show' implementation for ipasir solvers.
-- Reads the solver signature and appends the address of the provided pointer.
-- Since the solvers state is not reconstructable from the information stored in the pointer itself
-- there is no way of having 'show' be the dual of 'read'.
ipasirShow :: IpasirSolver s => IpasirSignature -> s -> String
ipasirShow ipasir_signature s = unsafeReadCString ipasir_signature ++ '@' : show (ptr s)

-- | Util function for turning signatures obtained through ffi into 'String'.
unsafeReadCString :: IO CString -> String
unsafeReadCString f = unsafePerformIO $ peekCString =<< f

-- | Creates a new ipasir solver form provided init and release methods.
-- The created 'ForeignPtr' will call the release method when it is
-- garbage collected.
ipasirNewIntSolver :: IpasirSolver s => IpasirInit -> IpasirRelease -> IO s
ipasirNewIntSolver ipasir_init ipasir_release = do
    p <- ipasir_init
    p' <- newForeignPtr ipasir_release p
    return $ ctr 0 p'

-- | 'addIntClause' implmenetation for ipasir solvers.
ipasirAddIntClause :: (Foldable f, IpasirSolver s)
                   => IpasirAdd    -- ^ 'ipasir_add' ffi binding
                   -> f (Lit Word) -- ^ clause
                   -> IntSolverAction s ()
ipasirAddIntClause ipasir_add clause = do
    s <- get
    lift $ do
        mapM_ (add s . fromLit) clause
        add s 0
    put $ ctr (newNumVars s) (ptr s)
  where
    newNumVars s = maximum $ (nVars s) : map extract (toList clause)
    add s i = withForeignPtr (ptr s) (`ipasir_add` toEnum i) 

-- | 'addIntAssumption' implmenetation for ipasir solvers.
ipasirAddIntAssumption :: IpasirSolver s
                       => IpasirAdd -- ^ 'ipasir_assume' ffi binding
                       -> Lit Word  -- ^ Assumption
                       -> IntSolverAction s ()
ipasirAddIntAssumption ipasir_assume l = do
    s <- get
    lift $ add s $ fromLit l
    put $ ctr (newNumVars s) (ptr s)
  where
    newNumVars s = nVars s `max` extract l
    add s i = withForeignPtr (ptr s) (`ipasir_assume` toEnum i) 

-- | 'intSolve' implementation for ipasir solvers.
-- This implementation does not adhere exactly to the ipasir.h contracts for 
-- 'ipasir_val' and 'ipasir_failed'.
-- Results from these two methods do not have to match the literal value exactly.
-- They only have to have the correct sign. Both methods will also only be called with
-- positve literals.
ipasirSolve :: IpasirSolver s
            => IpasirSolve
            -> IpasirVal
            -> IpasirFailed
            -> IntSolverAction s (ESolution Word)
ipasirSolve ipasir_solve ipasir_val ipasir_failed = do
    s <- get
    lift $ ioSolve s
  where
    ioSolve s = do
        sat <- withForeignPtr (ptr s) ipasir_solve
        case sat of
            10 -> ESolution <$> readSolution
            20 -> EConflict <$> readConflict
            _  -> error $ "ipasir_solve returned unexpected result: " ++ show sat
      where
        vars :: (Enum a, Num a) => [a]
        vars = [1 .. (toEnum $ fromEnum $ (nVars s) + 1)]
        readAll op = mapM (\ i -> withForeignPtr (ptr s) (`op` i)) vars
        assocVars = zip (map (\a -> a-1) vars)
        
        readSolution :: IO (Solution Word)
        readSolution = do
            rawSolution <- readAll ipasir_val
            return $ Solution $ Vec.fromList $ assocVars $ map litToLBool rawSolution
        
        readConflict :: IO (Conflict Word)
        readConflict = do
            rawConflict <- readAll ipasir_failed
            return $ Conflict $ Vec.fromList $ map fst $ filter ((== 1) . snd) $ assocVars rawConflict
        litToLBool i
            | i > 0     = LTrue
            | i < 0     = LFalse
            | otherwise = LUndef
