module SAT.Types
    ( module Export
    , Conflict
    , Solution
    , ESolution
    , AllSlutions
    ) where

import SAT.Types.LBool as Export
    ( LBool (..)

    , explode
    , explode'

    , shrink
    , shrink'

    , boolToLBool
    , maybeToLBool
    , lBoolToChar
    )

import SAT.Types.Lit as Export
    ( Lit (..)
    , IsLit (..)
    , lit
    , boolToLit
    , negLit
    , isLitPositive
    )

import Data.Set (Set)
import Data.Map (Map)


type Conflict v = Set v
type Solution v = Map v LBool
type ESolution v = Either (Conflict v) (Solution v)
type AllSlutions v = (Conflict v, [Solution v])