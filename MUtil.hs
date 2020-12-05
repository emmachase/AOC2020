module MUtil where

import Data.Char (digitToInt)
import Data.List (foldl')

lift1List :: (a -> b) -> [a] -> (b, [a])
lift1List f (x:xs) = (f x, xs)

lift2List :: (a -> b) -> ([a] -> c) -> [a] -> (b, c)
lift2List f g (x:xs) = (f x, g xs)

uncurryList :: [a] -> (a, [a])
uncurryList (x:xs) = (x, xs)

applyTuple :: (a -> b) -> (a, c) -> (b, c)
applyTuple f (x, y) = (f x, y)

nth :: [a] -> Int -> Maybe a
nth [] _ = Nothing
nth (x:xs) y  | y <= 0 = Just x
              | otherwise = nth xs (y-1)

nthM :: Maybe [a] -> Int -> Maybe a
nthM = (. flip nth) . (>>=)

fromRight :: Either a b -> b
fromRight (Right x) = x

first :: (a -> Bool) -> [a] -> Maybe a
first f xs = exists (filter f xs)
    where exists []    = Nothing
          exists (x:_) = Just x

checkRange :: (Num a, Ord a) => a -> a -> a -> Bool
checkRange min max x = (x >= min) && (x <= max)

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

getInput :: IO String
getInput = readFile "input.txt"

getInputs :: IO [String]
getInputs = lines <$> getInput

(<$$>) :: (Functor f, Functor f') => (a -> b) -> f (f' a) -> f (f' b)
(<$$>) = (<$>) . (<$>)
infixl 4 <$$>
