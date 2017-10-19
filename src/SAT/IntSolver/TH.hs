-- |
-- Provides ways to generate 'IntSolver' implementations for C interfaces. The
-- implementation may be generated for an arbitrary C interface using 'ffiSolver'
-- or for the <https://baldur.iti.kit.edu/sat-race-2015/downloads/ipasir.h ipasir.h>
-- interface using 'ipasirSolver'.
--
-- = Ipasir example
--
-- Unfortunately Template Haskell is currently unable to generate ffi function definitions,
-- thus generating an ipasir Solver still involves some amount of boilerplate.
--
-- This Module defines types for ipasir ffi function signatures that are expected by the
-- 'ipasirSolver' function.
--
-- @
--     {-\# LANGUAGE TemplateHaskell \#-}
--     module SAT.MySolver
--         ( MySolver
--         , mySolver
--         ) where
--     
--     import SAT.IntSolver.TH
--     
--     import Foreign.Ptr ( Ptr )
--     import Foreign.C.Types ( CInt( CInt ), CChar ( CChar ) )
--     
--     
--     foreign import ccall unsafe "ipasir.h  ipasir_signature" ipasir_signature :: IpasirSignature
--     foreign import ccall unsafe "ipasir.h  ipasir_init"      ipasir_init      :: IpasirInit
--     foreign import ccall unsafe "ipasir.h &ipasir_release"   ipasir_release   :: IpasirRelease
--     foreign import ccall unsafe "ipasir.h  ipasir_add"       ipasir_add       :: IpasirAdd
--     foreign import ccall unsafe "ipasir.h  ipasir_assume"    ipasir_assume    :: IpasirAssume
--     foreign import ccall unsafe "ipasir.h  ipasir_solve"     ipasir_solve     :: IpasirSolve
--     foreign import ccall unsafe "ipasir.h  ipasir_val"       ipasir_val       :: IpasirVal
--     foreign import ccall unsafe "ipasir.h  ipasir_failed"    ipasir_failed    :: IpasirFailed
--     
--     $(ipasirSolver "MySolver"
--         [e| ipasir_signature |]
--         [e| ipasir_init      |]
--         [e| ipasir_release   |]
--         [e| ipasir_add       |]
--         [e| ipasir_assume    |]
--         [e| ipasir_solve     |]
--         [e| ipasir_val       |]
--         [e| ipasir_failed    |])
-- @
--     

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module SAT.IntSolver.TH
    ( ffiSolver
    , ipasirSolver
    , ffiSolverShow
    , ipasirNewIntSolver
    , ipasirAddIntClause
    , ipasirSolve
    , unsafeReadCString
    
    , IpasirSignature
    , IpasirInit
    , IpasirRelease
    , IpasirAdd
    , IpasirAssume
    , IpasirSolve
    , IpasirVal
    , IpasirFailed
    ) where

import Data.Char ( toLower )
import Data.Proxy ( Proxy ( Proxy ) )

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Foldable ( mapM_, toList )

import Control.Monad.Trans.State.Lazy ( get, put )
import Control.Monad.Trans.Class ( lift )

import System.IO.Unsafe ( unsafePerformIO )
import Language.Haskell.TH.Syntax hiding ( Lit, lift )
import Language.Haskell.TH hiding ( Lit )

import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr, withForeignPtr )
import Foreign.C.Types ( CInt )
import Foreign.C.String ( CString, peekCString )

import SAT.IntSolver
import SAT.Types ( ESolution, Solution, Conflict, isLitPositive, LBool (..), IsLit (..) )
import Control.Comonad ( extract )
    
-- |
-- The ipasir interface defines three states a solver may be in. The interface itself
-- does not provide any way to retrieve this state but each method defines a state transition.
data IpasirState
    = INPUT -- ^ initial state. Also this state is entered if 'ipasir_add' or 'ipasir_assume' is called from 'SAT' or 'UNSAT'.
    | SAT   -- ^ reached if 'ipasir_solve' returns '10'.
    | UNSAT -- ^ reached if 'ipasir_solve' returns '20'.
        deriving (Eq, Show, Read)

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

-- | class that provides easier ways to interact with generated solver datatypes.
class (IntSolver a, Show a) => IpasirSolver a where
    nVars :: a -> Word
    ptr :: a -> ForeignPtr ()
    ctr :: Word -> ForeignPtr () -> a

-- | Generates a new datatype with the given name and  implementation of 'IntSolver'
-- from given ipasir functions.
-- 
-- Since 'ipasir.h' is rather inefficient when it comes to adding clauses and reading models
-- 'ffiSolver' should be used if more means of adding clauses or reading models can be provided.
-- 
-- = Example usage
--
-- Configure your buildsystem so that it includes the library providing the ipasir implementation.
-- Then create a module for your solver like so:
--
-- Unfortunately Template Haskell is currently unable to generate ffi function definitions,
-- thus generating an ipasir Solver still involves some amount of boilerplate.
--
-- @
--     {-\# LANGUAGE TemplateHaskell \#-}
--     module SAT.MySolver
--         ( MySolver
--         , mySolver
--         ) where
--     
--     import SAT.IntSolver.TH
--     
--     import Foreign.Ptr ( Ptr )
--     import Foreign.C.Types ( CInt( CInt ), CChar ( CChar ) )
--     
--     
--     foreign import ccall unsafe "ipasir.h  ipasir_signature" ipasir_signature :: IpasirSignature
--     foreign import ccall unsafe "ipasir.h  ipasir_init"      ipasir_init      :: IpasirInit
--     foreign import ccall unsafe "ipasir.h &ipasir_release"   ipasir_release   :: IpasirRelease
--     foreign import ccall unsafe "ipasir.h  ipasir_add"       ipasir_add       :: IpasirAdd
--     foreign import ccall unsafe "ipasir.h  ipasir_assume"    ipasir_assume    :: IpasirAssume
--     foreign import ccall unsafe "ipasir.h  ipasir_solve"     ipasir_solve     :: IpasirSolve
--     foreign import ccall unsafe "ipasir.h  ipasir_val"       ipasir_val       :: IpasirVal
--     foreign import ccall unsafe "ipasir.h  ipasir_failed"    ipasir_failed    :: IpasirFailed
--     
--     $(ipasirSolver "MySolver"
--         [e| ipasir_signature |]
--         [e| ipasir_init      |]
--         [e| ipasir_release   |]
--         [e| ipasir_add       |]
--         [e| ipasir_assume    |]
--         [e| ipasir_solve     |]
--         [e| ipasir_val       |]
--         [e| ipasir_failed    |])
-- @
--
-- 'ipasirSolver' will create implementations for 'show', 'newIntSolver', 'addIntClause' and 'solve' 
-- and pass those to 'ffiSolver'
--
ipasirSolver :: String -- ^ ADT and Constructor name
             -> Q Exp  -- ^ [e| ipasir_signature |]
             -> Q Exp  -- ^ [e| ipasir_init      |]
             -> Q Exp  -- ^ [e| ipasir_release   |]
             -> Q Exp  -- ^ [e| ipasir_add       |]
             -> Q Exp  -- ^ [e| ipasir_assume    |]
             -> Q Exp  -- ^ [e| ipasir_solve     |]
             -> Q Exp  -- ^ [e| ipasir_val       |]
             -> Q Exp  -- ^ [e| ipasir_failed    |]
             -> Q [Dec]
ipasirSolver name
    ipasir_signature
    ipasir_init
    ipasir_release
    ipasir_add
    ipasir_assume
    ipasir_solve
    ipasir_val
    ipasir_failed = ffiSolver name
        (ffiSolverShow [e| unsafeReadCString $ipasir_signature |])
        (ipasirNewIntSolver ipasir_init ipasir_release)
        (ipasirAddIntClause ipasir_add)
        (ipasirSolve ipasir_solve ipasir_val ipasir_failed)

-- | Create an 'IntSolver' implementation from given name and implementations for 'show', 'newIntSolver', 'addIntClause' and 'solve'
-- @$(ffiSolver "MySolver" ffiShow ffiNewIntSolver ffiAddIntClause ffiSolve)@
-- will result in the following code.
--
-- @
--    data MySolver = MySolver Word (ForeignPtr ())
--    mySolver = Proxy :: Proxy MySolver
--    instance IpasirSolver MySolver where
--        nVars (MySolver n _) = n
--        ptr (MySolver _ p) = p
--        ctr = MySolver
--    instance Show MySolver where
--        show = ffiShow . ptr
--    instance IntSolver MySolver where
--        newIntSolver _ = ctr 0 <$> ffiNewIntSolver
--        addIntClause c = do
--            solver <- get
--            newSolver <- lift (ffiAddIntClause solver c)
--            put newSolver
--        numVars = nVars <$> get
--        solve = do
--            solver <- get
--            lift ( ffiSolve solver )
-- @
--        
ffiSolver :: String -- ^ Name of the solver
          -> Q Exp  -- ^ implementation of 'show'
          -> Q Exp  -- ^ implementation of 'newIntSolver'
          -> Q Exp  -- ^ implementation of 'addIntClause'
          -> Q Exp  -- ^ implementation of 'solve'
          -> Q [Dec]
ffiSolver name ffiShow ffiNewIntSolver ffiAddClause ffiSolve = do
    let (dataName, ctr, extractNumVars, extractPtr, solverData) = createSolverData name
    impl <- [d|

            instance Show $(conT dataName) where
                show = $ffiShow . $extractPtr
            instance IpasirSolver $(conT dataName) where
                nVars = $extractNumVars
                ptr = $extractPtr
                ctr = $ctr
            instance IntSolver $(conT dataName) where
                newIntSolver _ = $ctr 0 <$> $ffiNewIntSolver
                addIntClause c = do
                    solver <- get
                    newSolver <- lift ($ffiAddClause solver c)
                    put newSolver
                numVars = nVars <$> get
                solve = do
                    solver <- get
                    lift ( $ffiSolve solver )

        |]
    
    return $ solverProxy name : solverData : impl

createSolverData :: String -> (Name, ExpQ, Q Exp, Q Exp, Dec)
createSolverData name = (dataName, ctr, field0, field1, dec)
  where
    dataName = mkName name
    ctr      = conE dataName
    field0   = nthField dataName 2 0
    field1   = nthField dataName 2 1
    nb = (Bang NoSourceUnpackedness NoSourceStrictness,)
    -- create 'data MySolver = MySolver Word (Ptr ())'
    dec = DataD [] dataName [] Nothing [
        NormalC dataName [nb $ ConT ''Word, nb $ AppT (ConT ''ForeignPtr) (ConT ''())]] [
        ConT $ mkName "Eq"]

    -- create '(\ (Ctr _ ... x ... _) -> x)'
    nthField name max n = do
        x <- newName "x"
        return $ LamE [ConP name (replicate n WildP ++ VarP x : replicate (max - n - 1) WildP)] $ VarE x

-- mySolver = Proxy :: Proxy MySolver
solverProxy name = FunD (mkName $ uncap name) [Clause [] (NormalB proxyExp) []]
  where
    -- Proxy :: Proxy MySolver
    proxyExp = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (ConT $ mkName name))
    uncap (c:cx) = toLower c : cx

-- | create a 'show' implmenetation for a Solver that consist of the provided solver signature and the hex value of its pointer.
-- @$(ffiSolverShow signature)@
-- will result in
-- @\ptr -> signature ++ "@" ++ show ptr@
ffiSolverShow :: Q Exp -> Q Exp
ffiSolverShow ipasir_signature = [e|
        \ptr -> $ipasir_signature ++ "@" ++ show ptr
    |]

-- | Util function for turning signatures obtained through ffi into 'String'.
unsafeReadCString :: IO CString -> String
unsafeReadCString f = unsafePerformIO $ peekCString =<< f

-- | Create a function that creates a 'ForeignPtr ()' with given constructor and destructor.
-- @$(ipasirNewIntSolver ctr dtr)@
-- will result in
-- @newForeignPtr dtr =<< ctr@
ipasirNewIntSolver :: Q Exp -- ^ constructor
                   -> Q Exp -- ^ destructor
                   -> Q Exp
ipasirNewIntSolver ipasir_init ipasir_release = [e|
        newForeignPtr $ipasir_release =<< $ipasir_init
    |]

-- | Create an 'addIntClause' implementation from 'ipasir_add' ffi function.
-- This will add all literals of the clause in order followed by '0'.
-- If the clause is empty only '0' is added (this should cause 'solve' to result in a conflict).
-- This function assumes that it is the only function calling 'ipasir_add'. It does not provide any
-- guaranties that the solver is currently in a state that accepts clauses. This shouldn't be a problem unless
-- the internal state of the solver is corrupted or its 'ipasir.h' implementation is defective.
--
-- 'nVars' of the new solver will be increased if the provided clause contains higher literals than the former
-- 'nVars'.
ipasirAddIntClause :: Q Exp -> Q Exp
ipasirAddIntClause ipasir_add = [e|
        ipasirAddIntClauseImpl $ipasir_add
    |]

ipasirAddIntClauseImpl :: (Foldable f, IsLit a Word, IpasirSolver s)
                       => IpasirAdd -- ^ 'ipasir_add' ffi binding
                       -> s         -- ^ the solver
                       -> f a       -- ^ clause
                       -> IO s      -- ^ the new solver
ipasirAddIntClauseImpl ipasir_add solver rawClause = do
    let clause = toLit <$> toList rawClause
    mapM_ (add . litToInt) clause
    add 0
    let newNumVars = maximum $ nVars solver : map extract clause 
    return $ ctr newNumVars $ ptr solver
  where
    litToInt lit = (fromEnum $ extract lit + 1) * if isLitPositive lit then 1 else -1
    add i = withForeignPtr (ptr solver) (`ipasir_add` toEnum i) 

-- | Creates a 'solve' implementation from 'ipasir_solve', 'ipasir_val' and 'ipasir_failed' ffi functions.
-- This assumes that all added clauses are terminated with a '0' and that the internal solver state is not
-- corrupted. This implementations makes no guarantees that this is the case.
--
-- If 'ipasir_solve' returns '10' a 'Solution' is created by calling 'ipasir_val' for each $i > nVars$.
-- If 'ipasir_solve' returns '20' a 'Conflict' is created by calling 'ipasir_failed' for each $i > nVars$.
-- All other return values of 'ipasir_solve' cause an 'error'.
ipasirSolve :: Q Exp -- ^ 'ipasir_solve'
            -> Q Exp -- ^ 'ipasir_val'
            -> Q Exp -- ^ 'ipasir_failed'
            -> Q Exp
ipasirSolve ipasir_solve ipasir_val ipasir_failed = [e|
        ipasirSolveImpl $ipasir_solve $ipasir_val $ipasir_failed
    |]

ipasirSolveImpl :: IpasirSolver s
                => IpasirSolve  -- ^ 'ipasir_solve'
                -> IpasirVal    -- ^ 'ipasir_val'
                -> IpasirFailed -- ^ 'ipasir_failed'
                -> s            -- ^ the solver
                -> IO (ESolution Word)
ipasirSolveImpl ipasir_solve ipasir_val ipasir_failed solver = do
    sat <- withForeignPtr (ptr solver) ipasir_solve
    case sat of
        10 -> Right <$> readSolution
        20 -> Left  <$> readConflict
        _  -> error $ "ipasir_solve returned unexpected result: " ++ show sat
  where
    vars :: (Enum a, Num a) => [a]
    vars = [1 .. (toEnum $ fromEnum $ nVars solver + 1)]
    readAll op = mapM (\ i -> withForeignPtr (ptr solver) (`op` i)) vars
    assocVars = zip (map (\a -> a-1) vars)

    readSolution :: IO (Solution Word)
    readSolution = do
        rawSolution <- readAll ipasir_val
        return $ Map.fromList $ assocVars $ map litToLBool rawSolution

    readConflict :: IO (Conflict Word)
    readConflict = do
        rawConflict <- readAll ipasir_failed
        return $ Set.fromList $ map fst $ filter ((== 1) . snd) $ assocVars rawConflict
    litToLBool i
        | i > 0     = LTrue
        | i < 0     = LFalse
        | otherwise = LUndef