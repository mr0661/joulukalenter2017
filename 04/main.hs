import Data.List
import Data.List.Split

--input = readFile "input.txt"
--exampleInput = [["aa", "bb","cc"],["ab", "ba","cc"]]
--a = ["ab", "ba","cc"]
parse x = map words ( splitOn "\n" x)

isValid1 [] = error "You forgot to put your input"
isValid1 [x] = True
isValid1 (x:xs) = not (elem x xs) && isValid1 xs

isValid2 [] = error "You forgot to put your input"
isValid2 [x] = True
isValid2 (x:xs) = not (elem (sort x) (map sort xs)) && isValid2 xs

count1 row
    | isValid1 row = 1
    | otherwise = 0

count2 row
    | isValid2 row = 1
    | otherwise = 0

countInput count [row] = count row
countInput count (row:rows) = count row + countInput count rows

getResultX x y = (countInput y) (parse x)
getResult1 x = getResultX x count1
getResult2 x = getResultX x count2

-- Get results --
main = do
    y <- readFile "input.txt"
    result1 <- return $ getResult1 y
    result2 <- return $ getResult2 y
    print result1
    print result2
    