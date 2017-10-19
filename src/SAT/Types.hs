-- | Provides Types used by 'SAT'.
module SAT.Types
    ( module SAT.Types.LBool
    , module SAT.Types.Lit
    , Conflict
    , Solution
    , ESolution
    , AllSlutions
    ) where

import Data.Set (Set)
import Data.Map (Map)

import SAT.Types.Lit
import SAT.Types.LBool


-- | Represents the result of an unsatisfiable formula. Contains all variables that are part of the conflict.
type Conflict v = Set v
-- | Represents the values for a satisfiable formula. Contains a truth value for each variable.
-- If the value is 'LUndef' either 'True' or 'False' are valid values.
type Solution v = Map v LBool
-- | Represents the solution or conflict of a formula
type ESolution v = Either (Conflict v) (Solution v)
-- | Represents all solutions for a formula as well as the first conflict.
type AllSlutions v = (Conflict v, [Solution v])