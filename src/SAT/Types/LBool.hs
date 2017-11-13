-- | 'LBool's are truth values that may be 'True', 'False' or its value may be irrelevant or unknown.
--
-- This module provides means to reason abouth such values.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SAT.Types.LBool
    ( LBool (..)

    , explode
    , explode'
    
    , shrink
    , shrink'
    
    , boolToLBool
    , maybeToLBool
    , lBoolToChar

    , LAll(..)
    , LAny(..)
    , applyAsBool
    ) where

import Control.Applicative ( Alternative ( (<|>), empty ) )

import GHC.Generics ( Generic )

import Data.Function ( on )
import Data.Monoid ( Monoid(..) )


-- | A Truth value that may be 'True', 'False' or irrelevant or unknown.
data LBool
    = LFalse -- ^ represents a 'False' value
    | LUndef -- ^ represents an unknown or irrelevant value.
    | LTrue  -- ^ represents a 'True' value
        deriving (Eq, Ord, Show, Read, Bounded, Generic)

-- | Any positive value maps to 'LTrue', any negative value maps to 'LFalse' and @0@ maps to 'LUndef'.
-- 'fromEnum' returns @1@, @-1@ and @0@ respectively.
instance Enum LBool where
    fromEnum LTrue  =  1
    fromEnum LFalse = -1
    fromEnum LUndef = 0
    toEnum i
        | i == 0    = LUndef
        | i <  0    = LFalse
        | otherwise = LTrue

-- | Create an alternative for each possible 'Bool'
explode :: Alternative f => LBool -> f Bool
explode = explode' True False

-- | Create an alternative for each possible value from provided representation of true and false.
explode' :: Alternative f 
         => a     -- ^ representation of true
         -> a     -- ^ representation of false
         -> LBool -- ^ @LBool@ to explode
         -> f a
explode' a _ LTrue  = pure a
explode' _ b LFalse = pure b
explode' a b LUndef = pure a <|> pure b

-- | Create an alternative for each exact 'Bool'
shrink :: Alternative f => LBool -> f Bool
shrink = shrink' True False

-- | Create an alternative for each exact value from provided representation of true and false.
shrink' :: Alternative f 
        => a     -- ^ representation of true
        -> a     -- ^ representation of false
        -> LBool -- ^ @LBool@ to shrink
        -> f a
shrink' a _ LTrue  = pure a
shrink' _ b LFalse = pure b
shrink' _ _ LUndef = empty

-- | Transform a 'Bool' to 'LBool'. 'True' maps to 'LTrue', 'False' maps to 'LFalse'.
boolToLBool :: Bool -> LBool
boolToLBool True  = LTrue
boolToLBool False = LFalse

-- | Transform a 'Maybe Bool' to 'LBool'. 'Nothing' maps to 'LUndef', 'Just True' maps to 'LTrue', 'Just False' maps tp 'LFalse'.
maybeToLBool :: Maybe Bool
             -> LBool
maybeToLBool = maybe LUndef boolToLBool

-- | provides a 'Char' that represents the truth value. 'LTrue' maps to @\'+\'@, 'LFalse' maps to @\'-\'@, 'LUndef' maps to @\'?\'@.
lBoolToChar :: LBool -> Char
lBoolToChar LTrue  = '+'
lBoolToChar LFalse = '-'
lBoolToChar LUndef = '?'


-- | Apply a function to the boolean representation of two @LBool@s.
-- If either of those is @LUndef@ the result is @LUndef
applyAsBool :: (Bool -> Bool -> Bool) -> LBool -> LBool -> LBool
applyAsBool f a b = maybeToLBool $ pure f <*> shrink a <*> shrink b

applyAsBoolBoxed :: (LBool -> a) -> (a -> LBool) 
                 -> (Bool -> Bool -> Bool)
                 -> a -> a -> a
applyAsBoolBoxed box unbox f = ((.).(.)) box (applyAsBool f) `on` unbox

-- | LBool monoid over Boolean under conjunction.
newtype LAll = LAll { getLAll :: LBool }
    deriving (Eq, Ord, Show, Enum, Read, Bounded, Generic)

instance Monoid LAll where
    mempty = LAll LTrue
    mappend = applyAsBoolBoxed LAll getLAll (&&)

-- | LBool monoid over Boolean under disjunction.
newtype LAny = LAny { getLAny :: LBool }
    deriving (Eq, Ord, Show, Enum, Read, Bounded, Generic)

instance Monoid LAny where
    mempty = LAny LFalse
    mappend = applyAsBoolBoxed LAny getLAny (||)
