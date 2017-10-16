{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module SAT.IntSolver.IPASIR.TH
    ( ffiSolver
    , ipasirSolver
    , ipasirShow
    , ipasirNewIntSolver
    , ipasirAddIntClause
    , ipasirSolve
    
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

import Control.Monad.Trans.State.Lazy
    ( StateT
    , get
    , put
    )
import Control.Monad.Trans.Class
    ( lift
    )

import System.IO.Unsafe ( unsafePerformIO )
import Language.Haskell.TH.Syntax hiding ( Lit, lift )
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH hiding ( Lit )

import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.ForeignPtr
    ( ForeignPtr
    , newForeignPtr
    , withForeignPtr
    )
import Foreign.C.Types ( CInt( CInt ), CChar( CChar ) )
import Foreign.C.String ( CString, peekCString )

import SAT.IntSolver
import SAT.Types
    ( ESolution
    , Solution
    , Conflict
    , Lit
    , isLitPositive
    , LBool (..)
    , IsLit (..)
    )
import Control.Comonad ( extract )
    
    
data IpasirState
    = INPUT
    | SAT
    | UNSAT
        deriving (Eq, Show, Read)

type IpasirSignature = IO CString
type IpasirInit      = IO (Ptr ())
type IpasirRelease   = FunPtr (Ptr() -> IO ())
type IpasirAdd       = Ptr () -> CInt -> IO ()
type IpasirAssume    = Ptr () -> CInt -> IO ()
type IpasirSolve     = Ptr () -> IO CInt
type IpasirVal       = Ptr () -> CInt -> IO CInt
type IpasirFailed    = Ptr () -> CInt -> IO CInt

class (IntSolver a, Show a) => IpasirSolver a where
    nVars :: a -> Word
    ptr :: a -> ForeignPtr ()
    ctr :: Word -> ForeignPtr () -> a

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
        (ipasirShow ipasir_signature)
        (ipasirNewIntSolver ipasir_init ipasir_release)
        (ipasirAddIntClause ipasir_add)
        (ipasirSolve ipasir_solve ipasir_val ipasir_failed)

ffiSolver :: String -> Q Exp -> Q Exp -> Q Exp -> Q Exp -> Q [Dec]
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

ipasirShow :: Q Exp -> Q Exp
ipasirShow ipasir_signature = [e|
        \ptr -> unsafePerformIO ( peekCString =<< $ipasir_signature ) ++ "@" ++ show ptr
    |]

ipasirNewIntSolver :: Q Exp -> Q Exp -> Q Exp
ipasirNewIntSolver ipasir_init ipasir_release = [e|
        newForeignPtr $ipasir_release =<< $ipasir_init
    |]

ipasirAddIntClause :: Q Exp -> Q Exp
ipasirAddIntClause ipasir_add = [e|
        ipasirAddIntClauseImpl $ipasir_add
    |]

ipasirAddIntClauseImpl :: (Foldable f, IsLit a Word, IpasirSolver s)
                       => IpasirAdd
                       -> s
                       -> f a
                       -> IO s
ipasirAddIntClauseImpl ipasir_add solver rawClause = do
    let clause = toLit <$> toList rawClause
    mapM_ (add . litToInt) clause
    add 0
    let newNumVars = maximum $ nVars solver : map extract clause 
    return $ ctr newNumVars $ ptr solver
  where
    litToInt lit = (fromEnum $ extract lit + 1) * if isLitPositive lit then 1 else -1
    add i = withForeignPtr (ptr solver) (`ipasir_add` toEnum i) 

ipasirSolve :: Q Exp -> Q Exp -> Q Exp -> Q Exp
ipasirSolve ipasir_solve ipasir_val ipasir_failed = [e|
        ipasirSolveImpl $ipasir_solve $ipasir_val $ipasir_failed
    |]

ipasirSolveImpl :: IpasirSolver s
                => IpasirSolve
                -> IpasirVal
                -> IpasirFailed
                -> s
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
    readSolution :: IO (Solution Word)
    readSolution = do
        rawSolution <- readAll ipasir_val
        return $ Map.fromList $ zip vars $ map litToLBool rawSolution
    readConflict :: IO (Conflict Word)
    readConflict = do
        rawConflict <- readAll ipasir_failed
        return $ Set.fromList $ map fst $ filter ((== 1) . snd) $ zip vars rawConflict
    litToLBool i
        | i > 0     = LTrue
        | i < 0     = LFalse
        | otherwise = LUndef