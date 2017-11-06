-- | Provides Types used by 'SAT'.
{-# LANGUAGE DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module SAT.Types
    ( module SAT.Types.LBool
    , module SAT.Types.Lit
    , Conflict(..)
    , conflictAsSet
    , Solution(..)
    , solutionAsMap
    , ESolution(..)
    , AllSolutions(..)
    , SomeSolutions(..)
    ) where

import GHC.Exts ( IsList ( Item, toList, fromList ) )
import GHC.Generics ( Generic )

import Control.Monad ( MonadPlus )
import Control.Applicative ( Alternative ( (<|>), empty ), liftA2 )

import Data.Functor.Classes
    ( Eq1   ( liftEq        )
    , Ord1  ( liftCompare   )
    , Show1 ( liftShowsPrec )
    )
import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
import Data.Monoid ( Monoid(..), (<>) )

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Vector ( Vector )

import SAT.Types.Lit
import SAT.Types.LBool


-- | Represents the values for a satisfiable formula. Contains a truth value for each variable.
-- If the value is 'LUndef' either 'True' or 'False' are valid values.
newtype Solution v = Solution (Vector (v, LBool)) deriving
    ( Eq, Ord, Show
    , Functor
    , Traversable, Foldable
    , Generic
    )

-- | View @Solution@ as @Map@
solutionAsMap :: Ord v => Solution v -> Map v LBool
solutionAsMap (Solution v) = Map.fromList $ toList v

instance IsList (Solution v) where
    type (Item (Solution v)) = (v, LBool)
    toList (Solution s) = toList s
    fromList = Solution . fromList

instance Eq1 Solution where
    liftEq eq (Solution s0) (Solution s1) = liftEq eq' s0 s1
      where
        eq' (v0, b0) (v1, b1) = eq v0 v1 && b0 == b1

instance Ord1 Solution where
    liftCompare comp (Solution s0) (Solution s1) = liftCompare comp' s0 s1
      where
        comp' (v0, b0) (v1, b1) = comp v0 v1 <> compare b0 b1

instance Show1 Solution where
    liftShowsPrec sPrec _ _ (Solution s) str = str ++ "Solution " ++ show (showLit <$> toList s)
      where
        showLit (v, b) = ", " ++ lBoolToChar b : "(" ++ (sPrec 0 v ")")

instance Applicative Solution where
    pure = Solution . pure . (,LTrue)
    (Solution fs) <*> (Solution s) = Solution $ (aug <$> fs) <*> s
      where
        aug (f, b0) (v, b1) = (f v, b2)
          where
            b2
              | b0 == b1                     = b0
              | b0 == LUndef || b1 == LUndef = LUndef
              | otherwise                    = LFalse

instance Alternative Solution where
    empty = Solution empty
    (Solution s0) <|> (Solution s1) = Solution $ s0 <|> s1


-- | Represents the result of an unsatisfiable formula. Contains all variables that are part of the conflict.
newtype Conflict v = Conflict (Vector v) deriving
    ( Eq, Ord, Show
    , Eq1, Ord1, Show1
    , Functor
    , Applicative, Alternative
    , Monad, MonadPlus
    , Foldable, Traversable
    , Generic
    )

-- | View @Conflict@ as @Set@
conflictAsSet :: Ord v => Conflict v -> Set v
conflictAsSet (Conflict v) = Set.fromList $ toList v

instance IsList (Conflict v) where
    type (Item (Conflict v)) = v
    toList (Conflict c) = toList c
    fromList = Conflict . fromList

-- | Represents the solution or conflict of a formula
data ESolution v
    = EConflict (Conflict v)
    | ESolution (Solution v)
        deriving ( Eq, Ord, Show, Functor, Generic )


-- | Represents all solutions for a formula as well as the first conflict.
data AllSolutions v = AllSolutions (Conflict v) [Solution v]
    deriving ( Eq, Ord, Show, Functor, Generic )

instance Eq1 AllSolutions where
    liftEq eq (AllSolutions c0 s0) (AllSolutions c1 s1) = liftEq eq c0 c1 && liftEq (liftEq eq) s0 s1

instance Ord1 AllSolutions where
    liftCompare comp (AllSolutions c0 s0) (AllSolutions c1 s1) = liftCompare comp c0 c1 <> liftCompare (liftCompare comp) s0 s1

-- | Represents some solutions for a formula and maybe a conflict
data SomeSolutions v = SomeSolutions (Maybe (Conflict v)) [Solution v]
    deriving ( Eq, Ord, Show, Functor, Generic )

instance Eq1 SomeSolutions where
    liftEq eq (SomeSolutions c0 s0) (SomeSolutions c1 s1) = liftEq (liftEq eq) c0 c1 && liftEq (liftEq eq) s0 s1

instance Ord1 SomeSolutions where
    liftCompare comp (SomeSolutions c0 s0) (SomeSolutions c1 s1) = liftCompare (liftCompare comp) c0 c1 <> liftCompare (liftCompare comp) s0 s1

instance Monoid (SomeSolutions v) where
    mempty = SomeSolutions Nothing empty
    mappend (SomeSolutions c0 s0) (SomeSolutions c1 s1) = SomeSolutions (c0 <|> c1) (s0 <|> s1)
instance Applicative SomeSolutions where
    pure = SomeSolutions Nothing . pure . pure
    (<*>) = liftA2' id
      where
        -- make toplevle for base >=4.10
        liftA2' f (SomeSolutions c0 s0) (SomeSolutions c1 s1) = SomeSolutions c2 s2
          where
            c2 = liftA2 (liftA2 f) c0 c1
            s2 = liftA2 (liftA2 f) s0 s1
