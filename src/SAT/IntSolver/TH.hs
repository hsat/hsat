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

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Data.Vector as Vec

import Data.Foldable ( mapM_, toList )
import Control.Monad.Trans.State.Lazy ( StateT(..), get )
import Control.Monad.Trans.Class ( lift )
import Control.Comonad ( extract )

import System.IO.Unsafe ( unsafePerformIO )
import Language.Haskell.TH
    ( Q, Exp (..), Dec (..), Type (..), Pat (..)
    , mkName, newName, conT
    , Con ( NormalC ), Bang ( Bang )
    , SourceUnpackedness(NoSourceUnpackedness)
    , SourceStrictness(NoSourceStrictness)
    )

import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr, withForeignPtr )
import Foreign.C.Types ( CInt )
import Foreign.C.String ( CString, peekCString )

import SAT.IntSolver
import SAT.Types
    ( ESolution(..), Solution(..), Conflict(..)
    , LBool (..), Lit, IsLit ( fromLit )
    )

    
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

-- | Generates a new datatype with the given name and  implementation of 'IntSolver'
-- from given ipasir functions.
-- 
-- Since 'ipasir.h' is rather inefficient when it comes to adding clauses and reading models
-- 'ffiSolver' should be used if more means of adding clauses or reading models can be provided.
-- Clause ( Clause )
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
        (ipasirAddIntAssumption ipasir_assume)
        (ipasirSolve ipasir_solve ipasir_val ipasir_failed)

-- | Create an 'IntSolver' implementation from given name and implementations for 'show', 'newIntSolver', 'addIntClause' and 'solve'
-- @$(ffiSolver "MySolver" ffiShow ffiNewIntSolver ffiAddIntClause ffiSolve)@
-- will result in the following code.
--
-- @
--    data MySolver = MySolver deriving (Eq, Ord, Show)
--    data MySolverIntState = MySolverIntState Word ForeignPtr ()
--        deriving (Eq, Ord)
--    instance Show MySolverIntState where
--        show (MySolverIntState _ ptr) = ffiShow ptr
--    instance IntSolver MySolverIntState where
--        type Marker MySolverIntState = MySolver
--        newIntSolver _ = MySolverIntState 0 <$> ffiNewIntSolver
--        addIntClause c = modifyM (`ffiAddIntClause` c)
--        addIntAssumption a = modifyM (`ffiAddAssumption` a)
--        numIntVars = do
--            (MySolverState nVars _) <- get
--            return nVars
--        solveInt = lift ( ffiSolve solver ) =<< get
-- @
--        
ffiSolver :: String -- ^ Name of the solver
          -> Q Exp  -- ^ implementation of 'show'
          -> Q Exp  -- ^ implementation of 'newIntSolver'
          -> Q Exp  -- ^ implementation of 'addIntClause'
          -> Q Exp  -- ^ implementation of 'addIntAssumption'
          -> Q Exp  -- ^ implementation of 'solve'
          -> Q [Dec]
ffiSolver name ffiShow ffiNewIntSolver ffiAddIntClause ffiAddAssumption ffiSolve = do
    let markerType = conT $ mkName name
    let markerData = createData name [] ["Eq", "Ord", "Show", "Generic"]
    ptrType <- [t| ForeignPtr () |]
    let intStateName = "Int" ++ name ++ "State"
    let intStateType = conT $ mkName intStateName
    let intStateData = createData intStateName [ConT ''Word, ptrType] ["Eq", "Ord", "Generic"]
    let nVarsField = field 0 intStateData
    let ptrField = field 1 intStateData
    impl <- [d|

            instance Show $(intStateType) where
                show = $ffiShow . $ptrField
            instance IntSolver $(intStateType) where
                type (Marker $(intStateType)) = $(markerType)
                newIntSolver _ = do
                    ptr <- $ffiNewIntSolver
                    return $ $(ctr intStateData) 0 ptr
                addIntClause c =
                    StateT (\ s -> (\ s' -> ((),s')) . uncurry $(ctr intStateData) <$> $ffiAddIntClause ($(nVarsField) s, $(ptrField) s) c)
                addIntAssumption a =
                    StateT (\ s -> (\ s' -> ((),s')) . uncurry $(ctr intStateData) <$> $ffiAddAssumption ($(nVarsField) s, $(ptrField) s) a)
                numIntVars = $(nVarsField) <$> get
                solveInt = do
                    s <- get
                    let solver = ($(nVarsField) s, $(ptrField) s)
                    lift ($ffiSolve solver)

        |]
    
    return $ markerData : intStateData : impl

-- | Create a declaration for an adt with a single constructor that is named the same as the type itself.
-- its arguments are the provided types. It derives all provided classes provided
createData :: String   -- ^ name of the adt and constructor
           -> [Type]   -- ^ types of the constructor arguments
           -> [String] -- ^ classes to derive
           -> Dec
createData name types classes = DataD [] dataName [] Nothing
    [ NormalC dataName (bang <$> types) ] derive
  where
    dataName = mkName name
    bang = (Bang NoSourceUnpackedness NoSourceStrictness, )
    derive = [ ConT $ mkName cls | cls <- classes]

ctr :: Dec -> Q Exp
ctr (DataD _ _ _ _ [NormalC ctrName _] _) = return $ ConE ctrName
ctr _ = error "ctr needs an adt declaration with exactly one constructor"

field :: Word -> Dec -> Q Exp
field n (DataD _ _ _ _ [NormalC ctrName ctrArgs] _) = do
    x <- newName "x"
    let p = ConP ctrName $ zipWith (toPat x) ctrArgs [0..]
    return $ LamE [p] $ VarE x
  where
    toPat x _ i
        | i == n    = VarP x
        | otherwise = WildP
field _ _ = error "field needs an adt declaration with exactly one constructor"


-- | create a 'show' implmenetation for a Solver that consist of the provided solver signature and the hex value of its pointer.
-- @$(ffiSolverShow signature)@
-- will result in
-- @\ptr -> signature ++ "@" ++ show ptr@
ffiSolverShow :: Q Exp -> Q Exp
ffiSolverShow ipasir_signature = [e|
        \sPtr -> $ipasir_signature ++ "@" ++ show sPtr
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

ipasirAddIntClauseImpl :: Foldable f
                       => IpasirAdd                -- ^ 'ipasir_add' ffi binding
                       -> (Word, ForeignPtr ())    -- ^ the solver
                       -> f (Lit Word)             -- ^ clause
                       -> IO (Word, ForeignPtr ()) -- ^ the new solver
ipasirAddIntClauseImpl ipasir_add (nVars, ptr) clause = do
    mapM_ (add . fromLit) clause
    add 0
    return (newNumVars, ptr)
  where
    newNumVars = maximum $ nVars : map extract (toList clause)
    add i = withForeignPtr ptr (`ipasir_add` toEnum i) 

-- | Create an 'addIntAssumption' implementation from 'ipasir_assume' ffi function.
-- 
-- 'nVars' of the new solver will be increased if the provided literal is higher
-- than the former 'nVars'.
ipasirAddIntAssumption :: Q Exp -> Q Exp
ipasirAddIntAssumption ipasir_assume = [e|
        ipasirAddIntAssumptionImpl $ipasir_assume
    |]

ipasirAddIntAssumptionImpl :: IpasirAdd                -- ^ 'ipasir_assume' ffi binding
                           -> (Word, ForeignPtr ())    -- ^ the solver
                           -> Lit Word                 -- ^ Assumption
                           -> IO (Word, ForeignPtr ()) -- ^ the new solver
ipasirAddIntAssumptionImpl ipasir_assume (numVars, ptr) l = do
    add $ fromLit l
    return (newNumVars, ptr)
  where
    newNumVars = max numVars (extract l)
    add i = withForeignPtr ptr (`ipasir_assume` toEnum i) 

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

ipasirSolveImpl :: IpasirSolve           -- ^ 'ipasir_solve'
                -> IpasirVal             -- ^ 'ipasir_val'
                -> IpasirFailed          -- ^ 'ipasir_failed'
                -> (Word, ForeignPtr ()) -- ^ the solver
                -> IO (ESolution Word)
ipasirSolveImpl ipasir_solve ipasir_val ipasir_failed (nVars, ptr) = do
    sat <- withForeignPtr ptr ipasir_solve
    case sat of
        10 -> ESolution <$> readSolution
        20 -> EConflict <$> readConflict
        _  -> error $ "ipasir_solve returned unexpected result: " ++ show sat
  where
    vars :: (Enum a, Num a) => [a]
    vars = [1 .. (toEnum $ fromEnum $ nVars + 1)]
    readAll op = mapM (\ i -> withForeignPtr ptr (`op` i)) vars
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
