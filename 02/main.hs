import Data.List.Split

splitLines x = splitOn "\n" x
splitTabs x = splitOn "\t" x

splitAllTabs [] = []
splitAllTabs [x] = [splitTabs x]
splitAllTabs (x:xs) = [splitTabs x] ++ splitAllTabs xs

parseInput x = map (map (read::String->Int)) $ splitAllTabs $ splitLines x

difference x = (maximum x) - (minimum x)

getChecksum1 [] = 0
getChecksum1 (x:xs) = difference x + getChecksum1 xs

--Is anything in y divisible by x
divisible x [] = 0
divisible x (y:ys)
    | x == y = divisible x ys
    | (mod x y == 0) = div x y
    | otherwise = divisible x ys

actualDivider [] y = 0
actualDivider (x:xs) y = max (divisible x y ) (actualDivider xs y)
divider x = actualDivider x x

getChecksum2 [] = 0
getChecksum2 (x:xs) = divider x + getChecksum2 xs

getResult1 x = getChecksum1(parseInput x)
getResult2 x = getChecksum2(parseInput x)

main = do
    y <- readFile "input.txt"
    result1 <- return $ getResult1 y
    result2 <- return $ getResult2 y
    print result1
    print result2