{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module SAT.Types.Lit
    ( Lit (..)
    , IsLit (..)
    , lit
    , boolToLit
    , negLit
    , isLitPositive
    ) where

import GHC.Generics ( Generic )

import Data.String ( IsString ( fromString ) )

import Data.Functor ( ($>) )
import Data.Functor.Classes

import Control.Monad ( ap )
import Data.Traversable ( Traversable ( traverse ) )

import Control.Comonad ( Comonad ( extract, duplicate ) )


-- | Represents a variable associated with a sign.
data Lit a
    = Pos a -- ^ a positive literal
    | Neg a -- ^ a negative literal
        deriving (Eq, Functor, Generic)

instance Eq1 Lit where
    liftEq f l r = isLitPositive l == isLitPositive r && f (extract l) (extract r)

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
            s = isLitPositive l == isLitPositive l'
            l' = f $ extract l

instance Comonad Lit where
    extract (Pos a) = a
    extract (Neg a) = a

    duplicate (Pos a) = Pos $ Pos a
    duplicate (Neg a) = Neg $ Neg a

instance Show a => Show (Lit a) where
    show (Pos a) = "+(" ++ show a ++ ")"
    show (Neg a) = "-(" ++ show a ++ ")"

instance IsString a => IsString (Lit a) where
    fromString = return . fromString

-- | Create a Literal with given sign
lit :: Bool -- ^ sign
    -> a    -- ^ value
    -> Lit a
lit True  = Pos
lit False = Neg

-- | Create an Empty Literal with given sign
boolToLit :: Bool -- ^ sign
          -> Lit ()
boolToLit = (`lit` ())

-- | Negate a literal.
negLit :: Lit a -> Lit a
negLit (Pos a) = Neg a
negLit (Neg a) = Pos a

-- | Get the sign of a Literal (True for positive)
-- @lit (isLitPositive l) (extract l) = l@
isLitPositive :: Lit a -> Bool
isLitPositive (Pos _) = True
isLitPositive (Neg _) = False

litFrom0 :: a -- ^ throws error
litFrom0 = error "can not create Lit from 0"

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

    toEnum 0 = litFrom0
    toEnum x = lit (x>0) (toEnum $ abs x)

-- | implemented to be able to create a 'Lit' from a numeric literal. 'fromInteger' fails on @0@.
instance (Num a) => Num (Lit a) where
    a + b = lit (isLitPositive a || isLitPositive b) (extract a + extract b)
    a * b = lit (isLitPositive a && isLitPositive b) (extract a * extract b)
    abs = Pos . abs . extract
    signum = fmap signum
    negate = negLit . fmap negate
    fromInteger 0 = litFrom0
    fromInteger i = lit (i > 0) (fromInteger $ abs i)

-- | @a@ may be treated as a @Lit l@.
-- the following has to hold:
--
-- @toLit . fromLit = id@
--
-- @fromLit . toLit = id@
--
-- 'toLit' may be undefined for values that do not carry a sign, like @0@.
class IsLit a l where
    toLit :: a -> Lit l
    fromLit :: Lit l -> a

instance IsLit Int Word where
    toLit 0 = litFrom0
    toLit i = lit (i > 0) $ toEnum $ (abs i) - 1
    fromLit (Pos i) =  1 + fromEnum i
    fromLit (Neg i) = -1 - fromEnum i

-- | identity
instance IsLit (Lit a) a where
    toLit   = id
    fromLit = id
