module SAT.Util
    ( prettyESolution
    , prettySolution
    , prettyConflict

    , boxESolution
    , boxSolution
    , boxConflict
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List ( intercalate )
import Data.Bifunctor ( bimap )

import Text.PrettyPrint.Boxes

import SAT.Types


prettyESolution :: Show a => ESolution a -> String
prettyESolution = render . boxESolution

boxESolution :: Show a => ESolution a -> Box
boxESolution = either boxConflict boxSolution


prettySolution :: Show a => Solution a -> String 
prettySolution = render . boxSolution

boxSolution :: Show a => Solution a -> Box
boxSolution = (text "Solution:" //) . vcat right . map (uncurry (<+>)) . map ( bimap (text . show) (char . lBoolToChar) ) . Map.toList


prettyConflict :: Show a => Conflict a -> String
prettyConflict = render . boxConflict

boxConflict :: Show a => Conflict a -> Box
boxConflict = (text "Conflict:" //) . vcat left . map (text . show) . Set.toList
