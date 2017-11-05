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


prettyESolution :: Show a => ESolution a -> String
prettyESolution = render . boxESolution

boxESolution :: Show a => ESolution a -> Box
boxESolution (ESolution s) = boxSolution s
boxESolution (EConflict s) = boxConflict s


prettySolution :: Show a => Solution a -> String 
prettySolution = render . boxSolution

boxSolution :: Show a => Solution a -> Box
boxSolution (Solution s) = text "Solution:" // (vcat right $ map (uncurry (<+>)) $ map ( swap . bimap (text . show) (char . lBoolToChar) ) $ toList s)


prettyConflict :: Show a => Conflict a -> String
prettyConflict = render . boxConflict

boxConflict :: Show a => Conflict a -> Box
boxConflict = (text "Conflict:" //) . vcat left . map (text . show) . toList


prettyVarCache :: Show a => VarCache a -> String
prettyVarCache = render . boxVarCache

boxVarCache :: Show a => VarCache a -> Box
boxVarCache vc = vcat right $ map toEntry [1 .. numVars vc]
  where
    toEntry :: Word -> Box
    toEntry i = text (show i) <> text (show (intToVar vc i))
