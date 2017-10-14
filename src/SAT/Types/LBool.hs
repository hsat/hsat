module SAT.Types.LBool
    ( LBool (..)

    , explode
    , explode'
    
    , shrink
    , shrink'
    
    , boolToLBool
    , maybeToLBool
    , lBoolToChar
    ) where

import Data.Maybe
    ( fromMaybe
    )

import Control.Applicative
    ( Alternative
        ( (<|>)
        , empty
        )
    )


data LBool
    = LFalse
    | LUndef
    | LTrue
        deriving (Eq, Ord, Show, Read)

instance Enum (LBool) where
    fromEnum LTrue  =  1
    fromEnum LFalse = -1
    fromEnum LUndef = 0
    toEnum i
        | i == 0    = LUndef
        | i <  0    = LFalse
        | otherwise = LTrue

explode :: Alternative f => LBool -> f Bool
explode = explode' True False

explode' :: Alternative f 
         => a     -- ^ representation of true
         -> a     -- ^ representation of false
         -> LBool -- ^ @LBool@ to explode
         -> f a
explode' a _ LTrue  = pure a
explode' _ b LFalse = pure b
explode' a b LUndef = pure a <|> pure b

shrink :: Alternative f => LBool -> f Bool
shrink = shrink' True False

shrink' :: Alternative f 
        => a     -- ^ representation of true
        -> a     -- ^ representation of false
        -> LBool -- ^ @LBool@ to shrink
        -> f a
shrink' a _ LTrue  = pure a
shrink' _ b LFalse = pure b
shrink' a b LUndef = empty

boolToLBool :: Bool -> LBool
boolToLBool True  = LTrue
boolToLBool False = LFalse

maybeToLBool :: Maybe Bool
             -> LBool
maybeToLBool = fromMaybe LUndef . fmap boolToLBool

lBoolToChar :: LBool -> Char
lBoolToChar LTrue  = '1'
lBoolToChar LFalse = '0'
lBoolToChar LUndef = '?'
