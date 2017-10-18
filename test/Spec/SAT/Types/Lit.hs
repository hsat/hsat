{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Spec.SAT.Types.Lit ( tests ) where

import Control.Comonad
    ( Comonad
        ( extract
        , extend
        , duplicate
        )
    )

import Control.Exception ( catch, SomeException, evaluate )

import Data.String ( IsString ( fromString ) )
import Data.List ( sort )

import Data.Functor.Identity ( Identity(..) )
import Data.Functor.Compose  ( Compose (..) )

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty.HUnit
import TestUtils

import SAT.Types


instance Serial m a => Serial m (Lit a)

tests = testGroup "Lit"
    [ testGroup "Ord"
        [ testProperty "Pos a > Neg a"          propOrdSign
        , testProperty "Pos a > Pos b => a > b" (propOrdValue Pos)
        , testProperty "Neg a > Neg b => a > b" (propOrdValue Neg)
        , testProperty "duals are neighbors"    propOrdNeighbors
        ]
    , testGroup "Foldable"
        [ testProperty "foldr f b l = f (extract l) b" propFoldableFoldr
        ]
    , testGroup "Traversable"
        [ testProperty "naturality"  propTraversableNaturality
        , testProperty "identity"    propTraversableIdentity
        , testProperty "composition" propTraversableComposition
        ]
    , testGroup "Monad"
        [ testProperty "neutral element1" propMonadLaw1
        , testProperty "neutral element2" propMonadLaw2
        , testProperty "composition" propMonadLaw3
        ]
    , testGroup "Comonad"
        [ testProperty "extend extract      = id" propComonadLaw1
        , testProperty "extract . extend f  = f" propComonadLaw2
        , testProperty "extend f . extend g = extend (f . extend g)" propComonadLaw3
        ]
    , testGroup "Show"
        [ testProperty "sign" propShowSign
        , testProperty "tail" propShowTail
        ]
    , testProperty "IsString" propIsString
    , testGroup "Enum"
        [ testProperty "fromEnum . toEnum = id" propEnumId1
        , testProperty "toEnum . fromEnum = id" propEnumId2
        , testCase     "toEnum 0 -> error" propEnumError
        , testProperty "extract . fromEnum =< 0 -> error" propEnumInnerError
        , testProperty "isLitPositive . toEnum == >0" propEnumSign
        ]
    , testGroup "utils"
        [ testProperty "lit" propUtilLit
        , testProperty "boolToLit" propUtilBoolToLit
        , testProperty "negLit" propUtilNegLit
        , testProperty "isLitPositive" propUtilIsLitPositive
        ]
    , testGroup "IsLit Int Word"
        [ testProperty "fromLit . toLit = id" propIsLitIntWordId1
        , testProperty "toLit . fromLit = id" propIsLitIntWordId2
        , testGroup "toLit"
            [ testCase "toLit 0 = error" propIsLitIntWordToLitError
            , testProperty "extract . toLit = (-1) . abs" propIsLitIntWordToLitValue
            , testProperty "isLitPositive . toLit = (> 0)" propIsLitIntWordToLitSign
            ]
        , testGroup "fromLit"
            [ testProperty "abs . fromLit = (+ 1)" propIsLitIntWordFromLitValue
            , testProperty "isLitPositive = (> 0) . fromLit" propIsLitIntWordFromLitSign
            ]
        ]
    , testGroup "IsLit (Lit a) a"
        [ testProperty "toLit = id" propIsLitLitToLit
        , testProperty "fromLit = id" propIsLitLitFromLit
        ]
    ]

propOrdSign :: Int -> Bool
propOrdSign i = Pos i > Neg i

propOrdValue :: (Int -> Lit Int) -> Int -> Int -> Bool
propOrdValue ctr a b = compare a b == compare (ctr a) (ctr b)

propOrdNeighbors :: [Lit Int] -> Bool
propOrdNeighbors ls = (extract <$> sort ls) == sort (extract <$> ls)


propFoldableFoldr :: Lit Int -> Int -> Bool
propFoldableFoldr l b = foldr (+) b l == (extract l) + b


propTraversableNaturality :: Lit Int -> Bool
propTraversableNaturality l = (t $ traverse f l) == (traverse (t . f) l)
  where
    t :: Maybe a -> [a]
    t = maybe [] return
    f :: Int -> Maybe Int
    f 0 = Nothing
    f i = Just $ i * 2

propTraversableIdentity :: Lit () -> Bool
propTraversableIdentity l = traverse Identity l == Identity l

propTraversableComposition :: Lit (Lit ()) -> Bool
propTraversableComposition l = traverse (Compose . fmap g . f) l == (Compose . fmap (traverse g) . traverse f) l
  where
    f = return :: a -> Maybe a
    g = return :: a -> [a]


propMonadLaw1 :: Int -> Bool
propMonadLaw1 a = (Pos a >>= Neg) == Neg a

propMonadLaw2 :: Lit () -> Bool
propMonadLaw2 m = (m >>= return) == m

propMonadLaw3 :: Lit () -> Bool
propMonadLaw3 m = ( m >>= (\x -> k x >>= h) ) == ( (m >>= k) >>= h )
  where
    k = Neg
    h = Pos


propComonadLaw1 :: Lit () -> Bool
propComonadLaw1 l = extend extract l == l

propComonadLaw2 :: Lit () -> Bool
propComonadLaw2 l = extract (extend f l) == f l
  where
    f = isLitPositive

propComonadLaw3 :: Lit () -> Bool
propComonadLaw3 l = (extend f . extend g) l == extend (f . extend g) l
  where
    g = isLitPositive
    f x = extract x && isLitPositive x


propShowSign :: Lit () -> Bool
propShowSign l = case head (show l) of
    '+' -> isLitPositive l
    '-' -> not $ isLitPositive l

propShowTail :: Lit Int -> Bool
propShowTail l = tail (show l) == "(" ++ show (extract l) ++ ")"


propIsString :: String -> Bool
propIsString s = extract (fromString s :: Lit String) == s


propUtilLit :: Bool -> Bool
propUtilLit s = isLitPositive (lit s ())  == s

propUtilBoolToLit :: Bool -> Bool
propUtilBoolToLit b = isLitPositive (boolToLit b) == b


propUtilNegLit :: Lit () -> Bool
propUtilNegLit l = isLitPositive (negLit l) == not (isLitPositive l)


propUtilIsLitPositive :: Lit () -> Bool
propUtilIsLitPositive l@(Pos _) = isLitPositive l
propUtilIsLitPositive l@(Neg _) = not $ isLitPositive l


propEnumId1 :: Int -> Bool
propEnumId1 0 = True
propEnumId1 i = fromEnum (toEnum i :: Lit Word) == i

propEnumId2 :: Lit Word -> Bool
propEnumId2 l = toEnum (fromEnum l) == l

try :: IO a -> IO (Either String a)
try action = catch (Right <$> action) (\e -> return $ Left $ head $ lines (show (e :: SomeException)))

propEnumError :: IO ()
propEnumError = do
    l <- try (evaluate $ (toEnum 0 :: Lit Word))
    assertEqual "error" (Left "Can't transform 0 into a Lit. (function toEnum).") l

propEnumInnerError l = monadic $ do
    i <- try (evaluate $ fromEnum (l :: Lit Int))
    return $ case i of
        (Left  _) -> extract l <= (0 :: Int)
        (Right n) -> abs (extract l) == abs n

propEnumSign :: Int -> Bool
propEnumSign 0 = True
propEnumSign i = isLitPositive (toEnum i :: Lit Word) == (i > 0)


propIsLitIntWordId1 :: Int -> Bool
propIsLitIntWordId1 0 = True
propIsLitIntWordId1 i = fromLit (toLit i :: Lit Word) == i

propIsLitIntWordId2 :: Lit Word -> Bool
propIsLitIntWordId2 l = toLit (fromLit l :: Int) == l

propIsLitIntWordToLitError = do
    (Left _) <- try (evaluate (toLit (0 :: Int) :: Lit Word))
    return ()

propIsLitIntWordToLitValue :: Int -> Bool
propIsLitIntWordToLitValue 0 = True
propIsLitIntWordToLitValue i = fromEnum (extract (toLit i :: Lit Word)) == abs i - 1

propIsLitIntWordToLitSign :: Int -> Bool
propIsLitIntWordToLitSign 0 = True 
propIsLitIntWordToLitSign i = isLitPositive (toLit i :: Lit Word) == (i > 0)

propIsLitIntWordFromLitValue :: Lit Word -> Bool
propIsLitIntWordFromLitValue l = abs (fromLit l :: Int) == fromEnum (extract l + 1)

propIsLitIntWordFromLitSign :: Lit Word -> Bool
propIsLitIntWordFromLitSign l = isLitPositive l == ((fromLit l :: Int) > 0)


propIsLitLitToLit :: Lit () -> Bool
propIsLitLitToLit l = toLit l == l

propIsLitLitFromLit :: Lit () -> Bool
propIsLitLitFromLit l = fromLit l == l
