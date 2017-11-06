-- | Provides utility functions for working with solutions.
module SAT.Util
    ( prettyESolution
    , prettySolution
    , prettyConflict
    , prettyVarCache

    , boxESolution
    , boxSolution
    , boxConflict
    , boxVarCache
    ) where

import Data.Tuple ( swap )
import Data.Bifunctor ( bimap )
import Data.Foldable ( toList )

import Text.PrettyPrint.Boxes

import SAT.Types
import SAT.Variables ( VarCache, intToVar, numVars )


-- | prints the solution as a table where each assignment is a row and the collumns are variables and values.
prettyESolution :: Show a => ESolution a -> String
prettyESolution = render . boxESolution

-- | boxes the solution as a table where each assignment is a row and the collumns are variables and values.
boxESolution :: Show a => ESolution a -> Box
boxESolution (ESolution s) = boxSolution s
boxESolution (EConflict s) = boxConflict s

-- | prints the solution as a table where each assignment is a row and the collumns are variables and values.
prettySolution :: Show a => Solution a -> String 
prettySolution = render . boxSolution

-- | boxes the solution as a table where each assignment is a row and the collumns are variables and values.
boxSolution :: Show a => Solution a -> Box
boxSolution (Solution s) = text "Solution:" // vcat right (map (uncurry (<+>) . swap . bimap (text . show) (char . lBoolToChar) ) $ toList s)

-- | prints the conflict as a list
prettyConflict :: Show a => Conflict a -> String
prettyConflict = render . boxConflict

-- | boxes the conflict as a list
boxConflict :: Show a => Conflict a -> Box
boxConflict = (text "Conflict:" //) . vcat left . map (text . show) . toList

-- | prints the varcache as a table where each row is a mapping and the collumns are integers and mapped variables.
prettyVarCache :: Show a => VarCache a -> String
prettyVarCache = render . boxVarCache

-- | boxes the varcache as a table where each row is a mapping and the collumns are integers and mapped variables.
boxVarCache :: Show a => VarCache a -> Box
boxVarCache vc = vcat right $ map toEntry [1 .. numVars vc]
  where
    toEntry :: Word -> Box
    toEntry i = text (show i) <> text (show (intToVar vc i))
