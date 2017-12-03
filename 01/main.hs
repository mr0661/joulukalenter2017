import Data.Char

charToInt :: Char -> Int
charToInt x = ord x - 48

stringToInt :: [Char] -> [Int]
stringToInt [x] = [charToInt x] 
stringToInt (x:xs) = (charToInt x) : stringToInt xs

compareArray :: Eq a => [a] ->[a] -> [Bool]
compareArray [] _ = []
compareArray _ [] = []
compareArray [][] = []
compareArray (x:xs) (y:ys) = [x == y] ++ compareArray xs ys

siftString :: (Eq t, Num t) => [a] -> t -> [a]
siftString (x:xs) y
    | y == 0 = x:xs
    | otherwise = siftString (xs ++ [x]) (y - 1)

prepareString1 x = siftString x 1
prepareString2 x = siftString x ((length x) `div` 2)

conditionalSum :: Num p => [p] -> [Bool] -> p
conditionalSum _[] = 0
conditionalSum []_ = 0
conditionalSum [][] = 0
conditionalSum (x:xs) (y:ys)
    | y = x + conditionalSum xs ys
    | otherwise = conditionalSum xs ys

getResultX x y = conditionalSum (stringToInt x) (compareArray x (y x))

getResult1 x = getResultX x prepareString1
getResult2 x = getResultX x prepareString2

-- Use getResult funtions or replace input with your own --
input = "This could be your input"
result1 = getResult1 input
result2 = getResult2 input