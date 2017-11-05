module SAT.Variables
    ( Var
    , VarCache
    , emptyCache
    , intToVar
    , varToInt
    , newVar
    , newVars
    , newHelper
    , newHelpers
    , numVars
    , vars
    , clausesToInt
    ) where

import Data.Vector ( Vector )
import qualified Data.Vector as Vec

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Maybe ( fromMaybe )


type Var l = Either Word l

-- | 
-- A VarCache manages a mapping between a chosen variable label type and
-- numeric Variables for a SAT-Solver.
-- The numeric values are assigend in ascending order.
--
-- A VariabelCache has to following 'laws':
--
-- @numVars emptyCache == 0@
--
-- @numVars $ snd $ newVar emptyCache v  = 1@
--
-- @fst $ newVar emptyCache v = Left v@
--
-- @numVars $ snd $ newHelper emptyCache = 1@
--
-- @fst $ newHelper emptyCache = Right i@
--
-- @intToVar . varToInt = id@
--
-- @varToInt . intToVar = id@
--
data VarCache v = VarCache
    { i2v :: Vector (Var v)
    , v2i :: Map (Var v) Word
    , nextHelper :: Word
    }

instance Show v => Show (VarCache v) where
    show = ("VarCache " ++) . show . v2i

-- | Create an empty cache of where the label type is @l@ and the Variable.
--   type is @Either l Word@.
--
--   This defaults to @emptyCache' Left Right@.
emptyCache :: VarCache v
emptyCache = VarCache Vec.empty Map.empty 0

-- | Insert a label into the cache and returns the resulting variable.
--   If the label is already in the cache it is returned.
newVar :: Ord v => VarCache v -> v -> (Var v, VarCache v)
newVar vc l = unsafeInsert vc $ Right l

-- | Inserts all labels into the cache and returns the resulting variables
newVars :: Ord v => VarCache v -> [v] -> ([Var v], VarCache v)
newVars vc = foldl newVars' ([], vc)
    where
        newVars' (vs, vc') v = (v':vs, vc'')
            where
                (v', vc'') = newVar vc' v

-- | Create a new helper variable.
newHelper :: Ord v => VarCache v -> (Var v, VarCache v)
newHelper vc = unsafeInsert vc' $ Left $ nextHelper vc
    where
        vc' = vc { nextHelper = nextHelper vc + 1 }

-- | Create @n@ new helper variables.
newHelpers :: Ord v => VarCache v -> Word -> ([Var v], VarCache v)
newHelpers vc nr = foldl newHelpers' ([], vc) [1..nr]
    where
        newHelpers' (vs, vc') _ = (v':vs, vc'')
            where
                (v', vc'') = newHelper vc'

-- | Check how many variables are in the cache.
numVars :: VarCache v -> Word
numVars = toEnum . length . i2v

-- | Extracts all variables from the cache.
vars :: VarCache v -> [Var v]
vars = Vec.toList . i2v

-- | Get the integer value for given variable.
--   This will cause an error if the variable is not in the cache.
varToInt :: Ord v => VarCache v -> Var v -> Word
varToInt vc v = (+ 1) $ fromMaybe (error "variable not in VarCache") $ Map.lookup v $ v2i vc

-- | Maps all variables in given clauses to integers using @varToInt@.
clausesToInt :: (Functor f2, Functor f1, Functor f, Ord v) => VarCache v -> f (f1 (f2 (Var v))) -> f (f1 (f2 Word))
clausesToInt vc = ((<$>).(<$>).(<$>)) (varToInt vc)

-- | Get the variable for given integer.
--   This will cause an error if the given integer is not associated with a variable.
intToVar :: VarCache v -> Word -> Var v
intToVar vc w = fromMaybe (error $ "VarCache has no variable for " ++ show w) $i2v vc Vec.!? (fromEnum w - 1) 

-- | Inserts a new Variable. This can also be a helper variable. 
--   This function does not increate the counter for the next helper variable.
--   This can result in some trouble. 
unsafeInsert :: Ord v => VarCache v -> Var v -> (Var v, VarCache v)
unsafeInsert vc v
    | Map.member v (v2i vc) = (v, vc)
    | otherwise             = (v, vc 
            { i2v = i2v vc `Vec.snoc` v
            , v2i = Map.insert v (numVars vc) (v2i vc)
            })
