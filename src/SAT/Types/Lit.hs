{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses #-}
module SAT.Types.Lit
    ( Lit (..)
    , IsLit (..)
    , lit
    , boolToLit
    , negLit
    , isLitPositive
    ) where

import Data.String
    ( IsString ( fromString )
    )

import Data.Bits
    ( xor
    )

import Data.Functor
    ( ($>)
    )

import Control.Monad
    ( ap
    )

import Data.Traversable
    ( Traversable ( traverse )
    )

import Control.Comonad
    ( Comonad
        ( extract
        , duplicate
        )
    )


data Lit a
    = Pos a
    | Neg a
        deriving (Eq, Functor)

-- | We order literals first by variable, then by sign, so that dual
-- literals are neighbors in the ordering.
instance (Ord a) => Ord (Lit a) where
    compare x y = shim x `compare` shim y
        where
            shim l = (extract l, isLitPositive l)

instance Foldable Lit where
    foldMap f = f . extract

instance Traversable Lit where
    traverse f l = (l $>) <$> f (extract l)

instance Applicative Lit where
    pure = Pos
    (<*>) = ap

instance Monad Lit where
    l >>= f = s `lit` extract l'
        where
            s = isLitPositive l `xor` isLitPositive l'
            l' = f $ extract l

instance Comonad Lit where
    extract (Pos a) = a
    extract (Neg a) = a

    duplicate (Pos a) = Pos $ Pos a
    duplicate (Neg a) = Neg $ Neg a

instance Show a => Show (Lit a) where
    show (Pos a) = '+' : show a
    show (Neg a) = '-' : show a

instance IsString a => IsString (Lit a) where
    fromString = return . fromString

-- | Create a Literal with given sign
lit :: Bool -> a -> Lit a
lit True  = Pos
lit False = Neg

-- | Create an Empty Literal with given sign
boolToLit :: Bool -> Lit ()
boolToLit = (`lit` ())

-- | Negate a literal.
negLit :: Lit a -> Lit a
negLit (Pos a) = Neg a
negLit (Neg a) = Pos a

-- | Get the sign of a Literal (True for positive)
isLitPositive :: Lit a -> Bool
isLitPositive (Pos _) = True
isLitPositive (Neg _) = False

instance (Enum a) => Enum (Lit a) where
    -- | Coerces a @Literal@ of a @Num@ @n@ to a signed @Int@ where the sign is the literals sign.
    -- for this to work @n > 0@ has to hold.
    fromEnum l = s * let n = fromEnum $ extract l in
            if n > 0
                then fromEnum n
                else error $ "can not coerce Literals with numbers <= 0 to Int"
        where
            s = if isLitPositive l
                then 1
                else -1

    toEnum 0 = error "Can't transform 0 into a Lit. (function toEnum)."
    toEnum x = lit (x>0) (toEnum $ abs x)

class IsLit a l where
    toLit :: a -> Lit l
    fromLit :: Lit l -> a

instance IsLit Int Word where
    toLit 0 = error "can not create Lit Word from 0"
    toLit i = lit (i > 0) $ toEnum $ (abs i) - 1
    fromLit (Pos i) =  1 + fromEnum i
    fromLit (Neg i) = -1 - fromEnum i
